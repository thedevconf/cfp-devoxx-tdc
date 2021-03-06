/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 Association du Paris Java User Group.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package library


import akka.actor._
import models._
import notifiers.Mails
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.WS
import play.libs.Akka
import play.api.Play.current

import scala.Predef._

/**
 * Akka actor that is in charge to process batch operations and long running queries
 *
 * Author: nicolas martignole
 * Created: 07/11/2013 16:20
 */

// This is a simple Akka event
case class ReportIssue(issue: Issue)

case class SendMessageToSpeaker(reporterUUID: String, proposal: Proposal, msg: String)

case class SendQuestionToSpeaker(visitorEmail: String, visitorName: String, proposal: Proposal, msg: String)

case class SendMessageToCommitte(reporterUUID: String, proposal: Proposal, msg: String)

case class SendMessageInternal(reporterUUID: String, proposal: Proposal, msg: String)

case class DraftReminder()

case class ComputeLeaderboard()

case class ComputeVotesAndScore()

case class RemoveVotesForDeletedProposal()

case class ProposalApproved(reporterUUID: String, proposal: Proposal)

case class ProposalRefused(reporterUUID: String, proposal: Proposal)

case class ProposalBackup(reporterUUID: String, proposal: Proposal)

case class SaveSlots(confType: String, slots: List[Slot], createdBy: Webuser)

case class LogURL(url: String, objRef: String, objValue: String)

case class NotifySpeakerRequestToTalk(authorUUiD: String, rtt: RequestToTalk)

case class EditRequestToTalk(authorUUiD: String, rtt: RequestToTalk)

case class NotifyProposalSubmitted(author: String, proposal: Proposal)

case class NotifyGoldenTicket(goldenTicket: GoldenTicket)

case class SendHeartbeat(apiKey: String, name: String)

case class SaveTDCSlots(trackId: String, slots: JsValue, createdBy: Webuser)

case class UpdateScheduleStatus(trackId: String, blocked: Boolean)

case class ProfileUpdated(speaker:Speaker)

case class ApprovedProposalUpdated(proposal:Proposal)

case class UploadPresentation(proposalId:String, filename:String, uploadedBy:String)

case class RequestSchedulePublication(trackId:String, requestedBy: Webuser)

case class RequestToUnlockSchedule(trackId:String, requestedBy: Webuser)

// Defines an actor (no failover strategy here)
object ZapActor {
  val actor = Akka.system.actorOf(Props[ZapActor])
}

class ZapActor extends Actor {

  def receive = {
    case ReportIssue(issue) => publishBugReport(issue)
    case SendMessageToSpeaker(reporterUUID, proposal, msg) => sendMessageToSpeaker(reporterUUID, proposal, msg)
    case SendMessageToCommitte(reporterUUID, proposal, msg) => sendMessageToCommitte(reporterUUID, proposal, msg)
    case SendMessageInternal(reporterUUID, proposal, msg) => postInternalMessage(reporterUUID, proposal, msg)
    case DraftReminder() => sendDraftReminder()
    case ComputeLeaderboard() => doComputeLeaderboard()
    case ComputeVotesAndScore() => doComputeVotesAndScore()
    case RemoveVotesForDeletedProposal() => doRemoveVotesForDeletedProposal()
    case ProposalApproved(reporterUUID, proposal) => doProposalApproved(reporterUUID, proposal)
    case ProposalRefused(reporterUUID, proposal) => doProposalRefused(reporterUUID, proposal)
    case ProposalBackup(reporterUUID, proposal) => doProposalBackup(reporterUUID, proposal)
    case SaveSlots(confType: String, slots: List[Slot], createdBy: Webuser) => doSaveSlots(confType: String, slots: List[Slot], createdBy: Webuser)
    case LogURL(url: String, objRef: String, objValue: String) => doLogURL(url: String, objRef: String, objValue: String)
    case NotifySpeakerRequestToTalk(authorUUiD: String, rtt: RequestToTalk) => doNotifySpeakerRequestToTalk(authorUUiD, rtt)
    case EditRequestToTalk(authorUUiD: String, rtt: RequestToTalk) => doEditRequestToTalk(authorUUiD, rtt)
    case NotifyProposalSubmitted(author: String, proposal: Proposal) => doNotifyProposalSubmitted(author, proposal)
    case SendHeartbeat(apiKey: String, name: String) => doSendHeartbeat(apiKey, name)
    case NotifyGoldenTicket(goldenTicket:GoldenTicket) => doNotifyGoldenTicket(goldenTicket)
    case SaveTDCSlots(trackId,slots,createdBy) => doSaveTDCSlots(trackId,slots,createdBy)
    case UpdateScheduleStatus(trackId, status) => doUpdateScheduleStatus (trackId, status)
    case ProfileUpdated(speaker) => doNotifyProfileUpdated(speaker)
    case UploadPresentation(proposalId,filename,uploadedBy) => doUploadPresentation(proposalId, filename,uploadedBy)
    case RequestSchedulePublication(trackId,requestedBy) => doRequestSchedulePublication(trackId,requestedBy)
    case RequestToUnlockSchedule(trackId,requestedBy) => doRequestToUnlockSchedule(trackId,requestedBy)
    case ApprovedProposalUpdated(proposal) => doNotifyApprovedProposalUpdated(proposal)
    case other => play.Logger.of("application.ZapActor").error("Received an invalid actor message: " + other)
  }

  def publishBugReport(issue: Issue) {
    if (play.Logger.of("application.ZapActor").isDebugEnabled) {
      play.Logger.of("application.ZapActor").debug(s"Posting a new bug report to Bitbucket")
    }

    // All the functional code should be outside the Actor, so that we can test it separately
    Issue.publish(issue)
  }

  def sendMessageToSpeaker(reporterUUID: String, proposal: Proposal, msg: String) {

    for (reporter <- Webuser.findByUUID(reporterUUID);
         speaker <- Webuser.findByUUID(proposal.mainSpeaker)) yield {
      Event.storeEvent(Event(proposal.id, reporterUUID, s"Sending a message to ${speaker.cleanName} about ${proposal.title}"))
      Mails.sendMessageToSpeakers(reporter, speaker, proposal, msg)
    }
  }

  def sendMessageToCommitte(reporterUUID: String, proposal: Proposal, msg: String) {
    Event.storeEvent(Event(proposal.id, reporterUUID, s"Sending a message to committee about ${proposal.id} ${proposal.title}"))
    Webuser.findByUUID(reporterUUID).map {
      reporterWebuser: Webuser =>
        Mails.sendMessageToCommitte(reporterWebuser, proposal, msg)
    }.getOrElse {
      play.Logger.error("User not found with uuid " + reporterUUID)
    }
  }

  def postInternalMessage(reporterUUID: String, proposal: Proposal, msg: String) {
    Event.storeEvent(Event(proposal.id, reporterUUID, s"Posted an internal message for ${proposal.id} ${proposal.title}"))
    Webuser.findByUUID(reporterUUID).map {
      reporterWebuser: Webuser =>
        Mails.postInternalMessage(reporterWebuser, proposal, msg)
    }.getOrElse {
      play.Logger.error("User not found with uuid " + reporterUUID)
    }
  }

  def sendDraftReminder() {
    val allProposalBySpeaker = Proposal.allDrafts().groupBy(_.mainSpeaker)
    allProposalBySpeaker.foreach {
      case (speaker: String, draftProposals: List[Proposal]) => {
        Webuser.findByUUID(speaker).map {
          speakerUser =>
            Mails.sendReminderForDraft(speakerUser, draftProposals)
        }.getOrElse {
          play.Logger.warn("User not found")
        }
      }
    }
  }

  def doComputeLeaderboard() {
    Leaderboard.computeStats()
  }

  def doComputeVotesAndScore() {
    Review.computeAndGenerateVotes()
    ReviewByGoldenTicket.computeAndGenerateVotes()
  }

  // Delete votes if a proposal was deleted
  def doRemoveVotesForDeletedProposal() {
    Proposal.allProposalIDsDeleted.map {
      proposalId =>
        Review.deleteVoteForProposal(proposalId)
        ReviewByGoldenTicket.deleteVoteForProposal(proposalId)
    }
  }

  def doProposalApproved(reporterUUID: String, proposal: Proposal) {
    for (reporter <- Webuser.findByUUID(reporterUUID);
         speaker <- Webuser.findByUUID(proposal.mainSpeaker)) yield {
      Event.storeEvent(Event(proposal.id, reporterUUID, s"Sent proposal Approved"))
      Mails.sendProposalApproved(speaker, proposal)
      Proposal.approve(reporterUUID, proposal.id)
    }
  }

  def doProposalBackup(reporterUUID: String, proposal: Proposal) {
    for (reporter <- Webuser.findByUUID(reporterUUID);
         speaker <- Webuser.findByUUID(proposal.mainSpeaker)) yield {
      Event.storeEvent(Event(proposal.id, reporterUUID, s"Sent proposal Backup"))
      Mails.sendProposalBackup(speaker, proposal)
      Event.backupNotificationSent(proposal)
    }
  }

  def doProposalRefused(reporterUUID: String, proposal: Proposal) {
    for (reporter <- Webuser.findByUUID(reporterUUID);
         speaker <- Webuser.findByUUID(proposal.mainSpeaker)) yield {
      Event.storeEvent(Event(proposal.id, reporterUUID, s"Sent proposal Refused"))
      Mails.sendProposalRefused(speaker, proposal)
      Proposal.reject(reporterUUID, proposal.id)
    }
  }

  def doSaveSlots(confType: String, slots: List[Slot], createdBy: Webuser) {
    ScheduleConfiguration.persist(confType, slots, createdBy)
  }

  def doLogURL(url: String, objRef: String, objValue: String) {
    HitView.storeLogURL(url, objRef, objValue)
  }

  def doNotifySpeakerRequestToTalk(authorUUID: String, rtt: RequestToTalk) {
    RequestToTalk.save(authorUUID, rtt)
    Mails.sendInvitationForSpeaker(rtt.speakerEmail, rtt.message.getOrElse("Hi, we would like to contact you for Devoxx."), rtt.id)
  }

  def doEditRequestToTalk(authorUUID: String, rtt: RequestToTalk) {
    RequestToTalk.save(authorUUID, rtt)
  }

  def doNotifyProposalSubmitted(author: String, proposal: Proposal) {
    Event.storeEvent(Event(proposal.id, author, s"Submitted a proposal ${proposal.id} ${proposal.title}"))
    Webuser.findByUUID(author).map {
      reporterWebuser: Webuser =>
        Mails.sendNotifyProposalSubmitted(reporterWebuser, proposal)
    }.getOrElse {
      play.Logger.error("User not found with uuid " + author)
    }
  }

  def doSendHeartbeat(apikey: String, name: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    // Check if redis is up... will throw an Exception if unable to connect
    Redis.checkIfConnected()

    // Send a Heartbeat to opsgenie
    val url = "https://api.opsgenie.com/v1/json/heartbeat/send"
    val futureResult = WS.url(url).post(Map("apiKey" -> Seq(apikey), "name" -> Seq(name)))
    futureResult.map {
      result =>
        result.status match {
          case 200 =>
            val json = Json.parse(result.body)
            if(play.Logger.of("library.ZapActor").isDebugEnabled){
              play.Logger.of("library.ZapActor").debug(s"Got an ACK from OpsGenie $json")
            }

          case other =>
            play.Logger.error(s"Unable to read response from OpsGenie server ${result.status} ${result.statusText}")
            play.Logger.error(s"Response body ${result.body}")

        }
    }
  }

  def doNotifyGoldenTicket(gt:GoldenTicket):Unit={
    play.Logger.debug(s"Notify golden ticket ${gt.ticketId} ${gt.webuserUUID}")

    Webuser.findByUUID(gt.webuserUUID).map {
      invitedWebuser: Webuser =>
         Event.storeEvent(Event(gt.ticketId, gt.webuserUUID, s"New golden ticket for user ${invitedWebuser.cleanName}"))
        Mails.sendGoldenTicketEmail(invitedWebuser,gt)
    }.getOrElse {
      play.Logger.error("Golden ticket error : user not found with uuid " + gt.webuserUUID)
    }
  }

  def doSaveTDCSlots(trackId: String, slots: JsValue, createdBy: Webuser): Unit = {
    TDCScheduleConfiguration.persist(trackId, slots, createdBy)
    Event.storeEvent(Event(trackId, createdBy.uuid, s"Updated track scheduling for track $trackId"))
    Mails.sendScheduleUpdated(trackId,createdBy.cleanName)
  }

  def doUpdateScheduleStatus(trackId:String, status:Boolean) = {
    TDCScheduleConfiguration.updateStatus(trackId, status)
  }

  def doNotifyProfileUpdated(speaker: Speaker) = {
    TDCClient.updateSpeakerProfile(speaker)
  }
  
  def doNotifyApprovedProposalUpdated(proposal: Proposal) = {
    TDCClient.updateApprovedProposal(proposal)
  }

  def doUploadPresentation(proposalId: String, filename: String, uploadedBy: String) = {
    val optProposal = Proposal.findById(proposalId)
    val trackId = optProposal.map(_.track).map(_.id).getOrElse("")
    val eventCode = ConferenceDescriptor.current().eventCode
    try {
      val proposal = optProposal.get
      val presentationUrl = S3.uploadPresentation(eventCode, trackId, filename)
      Event.storeEvent(Event(proposalId, uploadedBy, s"Uploaded presentation for track $trackId"))
      if(proposal.state == ProposalState.APPROVED || proposal.state == ProposalState.ACCEPTED) {
        TDCClient.updatePresentation(proposal, uploadedBy, ProposalURLs(Option(presentationUrl),None,None) )
      }
    } catch {
      case e:Exception =>
        Event.storeEvent(Event(proposalId, uploadedBy, s"Error uploading presentation for track $trackId : ${e.getMessage}"))
    }
  }
  
  def doRequestSchedulePublication(trackId: String, requestedBy: Webuser) = {
     Event.storeEvent(Event(trackId, requestedBy.uuid, s"Request for the organization to publish schedule for track $trackId"))
     Mails.sendRequestSchedulePublication(trackId)
  }

  def doRequestToUnlockSchedule(trackId: String, requestedBy: Webuser) = {
     Event.storeEvent(Event(trackId, requestedBy.uuid, s"Request for the organization to unlock schedule for track $trackId"))
     Mails.doRequestToUnlockSchedule(trackId)
  }
}

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

package notifiers

import com.typesafe.plugin._
import models._
import org.joda.time.{DateTime, DateTimeZone}
import play.api.Play.current
import play.api.i18n.{Messages,Lang}

/**
 * Sends all emails
 *
 * Author: nmartignole
 * Created: 04/10/2013 15:56
 */

object Mails {

  lazy val from = ConferenceDescriptor.current().fromEmail
  lazy val committeeEmail = ConferenceDescriptor.current().committeeEmail
  lazy val bugReportRecipient = ConferenceDescriptor.current().bugReportRecipient
  lazy val bcc = ConferenceDescriptor.current().bccEmail
  lazy val trackleadersEmail = ConferenceDescriptor.current().trackleadersEmail

  val cfpLang = sys.env.get("CFP_LANG").getOrElse("pt")
  implicit val lang = Lang(cfpLang)
  
  def sendResetPasswordLink(email: String, resetUrl: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val timestamp: String = new DateTime().toDateTime(DateTimeZone.forID("America/Sao_Paulo")).toString("HH:mm dd/MM")
    val subject:String = Messages("mail.reset_password_link.subject",timestamp,ConferenceDescriptor.current().naming.shortTitle+" CFP")
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(email)
    // If you want to receive a copy for validation
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(views.txt.Mails.sendResetLink(resetUrl).toString(), views.html.Mails.sendResetLink(resetUrl).toString)
  }

  def sendAccessCode(email: String, code: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val subject:String = Messages("mail.access_code.subject", ConferenceDescriptor.current().naming.shortTitle+" CFP")
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(email)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendAccessCode(email, code).toString(),
      views.html.Mails.sendAccessCode(email, code).toString
    )
  }

  def sendWeCreatedAnAccountForYou(email: String, firstname: String, tempPassword: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val subject: String = Messages("mail.account_created.subject", ConferenceDescriptor.current().naming.shortTitle+" CFP")
    emailer.setSubject(subject)
    emailer.addFrom(from)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.addRecipient(email)
    emailer.setCharset("utf-8")
    emailer.send(views.txt.Mails.sendAccountCreated(firstname, email, tempPassword).toString(), views.html.Mails.sendAccountCreated(firstname, email, tempPassword).toString)
  }

  def sendValidateYourEmail(email: String, validationLink: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val conferenceName=ConferenceDescriptor.current().naming.shortTitle+" CFP"
    val subject: String = Messages("mail.email_validation.subject", conferenceName)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.addRecipient(email)
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendValidateYourEmail(validationLink, conferenceName).toString(),
      views.html.Mails.sendValidateYourEmail(validationLink, conferenceName).toString()
    )
  }

  def sendBugReport(bugReport: Issue) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val subject: String = Messages("mail.issue_reported.subject")
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addCc(bugReport.reportedBy)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.addRecipient(bugReportRecipient)
    emailer.setCharset("utf-8")
    emailer.send(
      views.html.Mails.sendBugReport(bugReport).toString(),
      views.html.Mails.sendBugReport(bugReport).toString()
    )
  }

  def sendMessageToSpeakers(fromWebuser: Webuser, toWebuser: Webuser, proposal: Proposal, msg: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.cfp_message_to_speaker.subject", proposal.id, Messages(proposal.track.label), eventCode)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.addRecipient(toWebuser.email)

    // The Java Mail API accepts varargs... Thus we have to concatenate and turn Scala to Java
    // I am a Scala coder, please get me out of here...
    val maybeSecondSpeaker = proposal.secondarySpeaker.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val maybeOtherEmails = proposal.otherSpeakers.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val listOfEmails = maybeOtherEmails ++ maybeSecondSpeaker.toList
    emailer.addCc(listOfEmails.toSeq: _*) // magic trick to create a java varargs from a scala List

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendMessageToSpeaker(fromWebuser.cleanName, proposal, msg).toString(),
      views.html.Mails.sendMessageToSpeaker(fromWebuser.cleanName, proposal, msg).toString()
    )

    // For Program committee
    emailer.setSubject(s"[${Messages(proposal.track.label)} ${proposal.id}]")
    emailer.addFrom(from)
    emailer.addRecipient(committeeEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    // Send also a copy of the message to the track leaders (included for TDC)
    addTrackLeadersEmails(proposal.track.id,emailer)

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendMessageToSpeakerCommittee(fromWebuser.cleanName, toWebuser.cleanName, proposal, msg).toString(),
      views.html.Mails.sendMessageToSpeakerCommitte(fromWebuser.cleanName, toWebuser.cleanName, proposal, msg).toString()
    )
  }

  def sendMessageToCommitte(fromWebuser: Webuser, proposal: Proposal, msg: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.speaker_message_to_cfp.subject", Messages(proposal.track.label), proposal.id, fromWebuser.cleanName)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(committeeEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")

    // Send also a copy of the message to the other speakers
    val maybeSecondSpeaker = proposal.secondarySpeaker.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val mainSpeaker = Webuser.getEmailFromUUID(proposal.mainSpeaker)
    val maybeOtherEmails = proposal.otherSpeakers.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val listOfEmails = mainSpeaker ++ maybeOtherEmails ++ maybeSecondSpeaker
    emailer.addCc(listOfEmails.toSeq: _*) // magic trick to create a java varargs from a scala List

    // Send also a copy of the message to the track leaders (included for TDC)
    addTrackLeadersEmails(proposal.track.id,emailer)


    emailer.send(
      views.txt.Mails.sendMessageToCommitte(fromWebuser.cleanName, proposal, msg).toString(),
      views.html.Mails.sendMessageToCommitte(fromWebuser.cleanName, proposal, msg).toString()
    )
  }

  def sendNotifyProposalSubmitted(fromWebuser: Webuser, proposal: Proposal) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.notify_proposal.subject", fromWebuser.cleanName, proposal.title)

    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(committeeEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendNotifyProposalSubmitted(fromWebuser.cleanName, proposal.id, proposal.title, Messages(proposal.track.label), Messages(proposal.talkType.id)).toString(),
      views.html.Mails.sendNotifyProposalSubmitted(fromWebuser.cleanName, proposal.id, proposal.title, Messages(proposal.track.label), Messages(proposal.talkType.id)).toString()
    )
  }

  def postInternalMessage(fromWebuser: Webuser, proposal: Proposal, msg: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    emailer.setSubject(s"[$eventCode PRIVATE][${proposal.title}]")
    emailer.addFrom(from)
    emailer.addRecipient(committeeEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    // Send also a copy of the message to the track leaders (included for TDC)
    addTrackLeadersEmails(proposal.track.id,emailer)

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.postInternalMessage(fromWebuser.cleanName, proposal, msg).toString(),
      views.html.Mails.postInternalMessage(fromWebuser.cleanName, proposal, msg).toString()
    )
  }
  /*
    Adds the emails of the trackleaders to the cc attribute of the email
   */
  private def addTrackLeadersEmails(trackId: String, emailer:MailerAPI): Unit = {
    val trackleadersEmails = TrackLeader.findAll(trackId)
                                        .flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    emailer.addCc(trackleadersEmails.toSeq: _*)
  }

  def sendReminderForDraft(speaker: Webuser, proposals: List[Proposal]) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    if (proposals.size == 1) {
      val subject: String = s"[$eventCode] " + Messages("mail.draft_single_reminder.subject", ConferenceDescriptor.current().naming.title)
      emailer.setSubject(subject)
    }
    if (proposals.size > 1) {
      val subject: String = s"[$eventCode] " + Messages("mail.draft_multiple_reminder.subject", proposals.size, ConferenceDescriptor.current().naming.title)
      emailer.setSubject(subject)
    }
    emailer.addFrom(from)
    emailer.addRecipient(speaker.email)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendReminderForDraft(speaker.firstName, proposals).toString(),
      views.html.Mails.sendReminderForDraft(speaker.firstName, proposals).toString()
    )
  }

  def sendProposalApproved(toWebuser: Webuser, proposal: Proposal) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.proposal_approved.subject", Messages(proposal.track.label), proposal.id)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(toWebuser.email)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    // The Java Mail API accepts varargs... Thus we have to concatenate and turn Scala to Java
    // I am a Scala coder, please get me out of here...
    val maybeSecondSpeaker = proposal.secondarySpeaker.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val maybeOtherEmails = proposal.otherSpeakers.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val listOfEmails = maybeOtherEmails ++ maybeSecondSpeaker.toList
    emailer.addCc(listOfEmails.toSeq: _*) // magic trick to create a java varargs from a scala List

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.acceptrefuse.sendProposalApproved(toWebuser.firstName, proposal).toString(),
      views.html.Mails.acceptrefuse.sendProposalApproved(toWebuser.firstName, proposal).toString()
    )
  }

  def sendProposalRefused(toWebuser: Webuser, proposal: Proposal) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.proposal_refused.subject", Messages(proposal.track.label), proposal.title)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(toWebuser.email)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    // The Java Mail API accepts varargs... Thus we have to concatenate and turn Scala to Java
    val maybeSecondSpeaker = proposal.secondarySpeaker.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val maybeOtherEmails = proposal.otherSpeakers.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val listOfEmails = maybeOtherEmails ++ maybeSecondSpeaker.toList
    emailer.addCc(listOfEmails.toSeq: _*) // magic trick to create a java varargs from a scala List

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.acceptrefuse.sendProposalRefused(toWebuser.firstName, proposal).toString(),
      views.html.Mails.acceptrefuse.sendProposalRefused(toWebuser.firstName, proposal).toString()
    )
  }

  def sendProposalBackup(toWebuser: Webuser, proposal: Proposal) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.proposal_backup.subject", Messages(proposal.track.label), proposal.title)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(toWebuser.email)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    // The Java Mail API accepts varargs... Thus we have to concatenate and turn Scala to Java
    val maybeSecondSpeaker = proposal.secondarySpeaker.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val maybeOtherEmails = proposal.otherSpeakers.flatMap(uuid => Webuser.getEmailFromUUID(uuid))
    val listOfEmails = maybeOtherEmails ++ maybeSecondSpeaker.toList
    emailer.addCc(listOfEmails.toSeq: _*) // magic trick to create a java varargs from a scala List

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.acceptrefuse.sendProposalBackup(toWebuser.firstName, proposal).toString(),
      views.html.Mails.acceptrefuse.sendProposalBackup(toWebuser.firstName, proposal).toString()
    )
  }

  def sendInvitationForSpeaker(speakerEmail: String, message: String, requestId: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val shortYearlyName = ConferenceDescriptor.current().naming.shortTitle
    emailer.setSubject(s"$shortYearlyName special request")
    emailer.addFrom(from)
    emailer.addRecipient(speakerEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))

    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendInvitationForSpeaker(message, requestId).toString(),
      views.html.Mails.sendInvitationForSpeaker(message, requestId).toString()
    )
  }

  def sendScheduleUpdated(trackId: String, author: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.schedule_updated.subject", Messages(s"${trackId}.label"))

    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(committeeEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendScheduleUpdated(trackId,author).toString(),
      views.html.Mails.sendScheduleUpdated(trackId,author).toString()
    )
  }

  def sendProfileUpdated(speaker: Speaker) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.profile_updated.subject", speaker.cleanName)

    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(committeeEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendProfileUpdated(speaker).toString(),
      views.html.Mails.sendProfileUpdated(speaker).toString()
    )
  }


  def sendGoldenTicketEmail(invitedWebuser: Webuser, gt: GoldenTicket) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.goldenticket.subject", ConferenceDescriptor.current().naming.shortTitle)
    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(invitedWebuser.email)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.goldenticket.sendGoldenTicketEmail(invitedWebuser, gt).toString(),
      views.html.Mails.goldenticket.sendGoldenTicketEmail(invitedWebuser, gt).toString()
    )
  }
  
  def sendRequestSchedulePublication(trackId: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.schedule_publication.subject", Messages(s"${trackId}.label"))

    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(trackleadersEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendRequestSchedulePublication(trackId).toString(),
      views.html.Mails.sendRequestSchedulePublication(trackId).toString()
    )
  }

 
  def doRequestToUnlockSchedule(trackId: String) = {
    val emailer = current.plugin[MailerPlugin].map(_.email).getOrElse(sys.error("Problem with the MailerPlugin"))
    val eventCode = ConferenceDescriptor.current().eventCode
    val subject: String = s"[$eventCode] " + Messages("mail.unlock_schedule.subject", Messages(s"${trackId}.label"))

    emailer.setSubject(subject)
    emailer.addFrom(from)
    emailer.addRecipient(trackleadersEmail)
    bcc.map(bccEmail => emailer.addBcc(bccEmail))
    emailer.setCharset("utf-8")
    emailer.send(
      views.txt.Mails.sendRequestToUnlockSchedule(trackId).toString(),
      views.html.Mails.sendRequestToUnlockSchedule(trackId).toString()
    )
  } 
 
}

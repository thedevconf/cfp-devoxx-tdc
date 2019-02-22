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

package controllers

import library.{UpdateScheduleStatus, SaveTDCSlots, ZapActor, RequestSchedulePublication, RequestToUnlockSchedule}
import models._
import play.api.i18n.Messages
import play.api.libs.json.Json


/**
  * TDCScheduling Controller.
  * Allows the scheduling of talks for each track in the TDC Conference
  */
object TDCSchedulingController extends SecureCFPController {

  def index = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      val allTracks = ConferenceDescriptor.ConferenceTracks.ALL
        .filter(track => Webuser.hasAccessToAdmin(uuid) | TrackLeader.isTrackLeader(track.id, uuid))
        .sortBy(track => track.label)
      Ok(views.html.Scheduling.scheduling(allTracks))
  }

  def saveSchedule(trackId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      request.body.asJson.map {
        json =>
          ZapActor.actor ! SaveTDCSlots(trackId, json, request.webuser)
          Ok("{\"status\":\"success\"}").as("application/json")
      }.getOrElse {
        BadRequest("{\"status\":\"expecting json data\"}").as("application/json")
      }
  }

  /**
    * returns a Json with all the tracks that have a schedule and their respective status
    *
    */
  def allScheduledTracks() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      val tracks:List[(Track,Boolean)] = TDCScheduleConfiguration.allScheduledTracks().toList
                  .map{case (trackId,schedule) => {
                    val track = Track.parse(trackId)
                    val updatedTrack = track.copy(label = Messages(track.label))
                    (updatedTrack,schedule.blocked.getOrElse(false))
                  }}
      val json = Json.toJson(Map(
                              "scheduledTracks" ->
                                    Json.toJson(tracks.map{case (track,blocked) =>
                                        Map(
                                          "id" -> Json.toJson(track.id),
                                          "label" -> Json.toJson(track.label),
                                          "blocked" -> Json.toJson(blocked)
                                        )
                                    })
                              )
      )
      Ok(Json.stringify(json)).as("application/json")
  }

  def deleteSchedule(trackId:String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      TDCScheduleConfiguration.delete(trackId)
      Ok("{\"status\":\"deleted\"}").as("application/json")
  }

  def loadSchedule(trackId:String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      import TDCScheduleConfiguration.scheduleConfigurationFormat

      //checks if the user has authorization to read the schedule for the track (is admin or trackleader of the track)
      val uuid = request.webuser.uuid
      if(Webuser.hasAccessToAdmin(uuid) | TrackLeader.isTrackLeader(trackId, uuid)) {

        //Loads all the proposals for the track
        val proposals = ApprovedProposal.allApproved().filter(p => p.track.id == trackId)
        val proposalsWithSpeaker = proposals.map {
          p: Proposal =>
            val mainWebuser = Speaker.findByUUID(p.mainSpeaker)
            val secWebuser = p.secondarySpeaker.flatMap(Speaker.findByUUID)
            val oSpeakers = p.otherSpeakers.map(Speaker.findByUUID)
            val company:String = mainWebuser.flatMap(_.company.map(_.toLowerCase.capitalize)).getOrElse("")

            // Transform speakerUUID to Speaker name, this simplify Angular Code
            p.copy(
              mainSpeaker = mainWebuser.map(_.cleanName).getOrElse("")
              , secondarySpeaker = secWebuser.map(_.cleanName)
              , otherSpeakers = oSpeakers.flatMap(s => s.map(_.cleanName))
              , talkType = p.talkType.copy(label = Messages(p.talkType.label+".simple"))
              , summary = company //uses the summary field to inform the company for the proposal 
              , privateMessage = if (p.state == ProposalState.ACCEPTED) "OK" else ""
              , state = ProposalState(Messages(p.state.code))
            )
        }

        //Loads a scheduled configuration for the track if it already exists
        val maybeScheduledConfiguration = TDCScheduleConfiguration.loadScheduledConfiguration(trackId)

        val jsonResult = maybeScheduledConfiguration match {
          case None =>
            Json.toJson(Map(
              "approvedTalks" -> Json.toJson(proposalsWithSpeaker)
            ))
          case Some(config) =>
            val selectedIds = config.slots.flatMap(slot => slot.proposals)
            val (scheduledProposals,availableProposals) = proposalsWithSpeaker.partition(p => selectedIds.contains(p.id))

            val fullSlots:List[FullTDCSlot] = config.slots.map(slot =>
              FullTDCSlot(slot.id, slot.stadium, slot.proposals.map(id =>
                scheduledProposals.find(_.id == id).getOrElse({
                  //for the case when the trackleader removes the approval of a scheduled proposal
                  val p = Proposal.findById(id).get
                  val mainWebuser = Speaker.findByUUID(p.mainSpeaker)
                  val secWebuser = p.secondarySpeaker.flatMap(Speaker.findByUUID)
                  val oSpeakers = p.otherSpeakers.map(Speaker.findByUUID)
                  p.copy(mainSpeaker = mainWebuser.map(_.cleanName).getOrElse("")
                    , secondarySpeaker = secWebuser.map(_.cleanName)
                    , otherSpeakers = oSpeakers.flatMap(s => s.map(_.cleanName))
                    , talkType = p.talkType.copy(label = Messages(p.talkType.label+".simple"))
                    , summary = ""
                    , privateMessage = if (p.state == ProposalState.ACCEPTED) "OK" else ""
                    , state = ProposalState(Messages(p.state.code))
                  )
                }))))

            val fullSchedule = TDCScheduleConfiguration(trackId,fullSlots,config.blocked)

            Json.toJson(Map(
              "approvedTalks" -> Json.toJson(availableProposals)
              ,"fullSchedule" -> Json.toJson(fullSchedule)
            ))
        }
        Ok(Json.stringify(jsonResult)).as("application/json")
     } else {
        Unauthorized("{\"status\":\"unauthorized\"}").as("application/json")
     }
  }

  /**
    * updates the blocked status of a schedule
    *
    * @param trackId
    * @return
    */
  def updateStatus(trackId:String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      request.body.asJson.map {
        json =>
          ZapActor.actor ! UpdateScheduleStatus(trackId, (json \ "blocked").as[Boolean] )
          Ok("{\"status\":\"success\"}").as("application/json")
      }.getOrElse {
        BadRequest("{\"status\":\"expecting json data\"}").as("application/json")
      }
  }
  
  def requestPublication() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      request.body.asJson.map {json => 
        val trackId = (json \ "trackId").as[String]
        ZapActor.actor ! RequestSchedulePublication(trackId, request.webuser)
        Ok("{\"status\":\"success\"}").as("application/json")
      }.getOrElse {
        BadRequest("{\"status\":\"expecting json data\"}").as("application/json")
      }
  }  
  /**
  * sends an email requesting that the schedule be unlocked so the trackleaders can make updates
  */
  def requestNotification() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      request.body.asJson.map {json => 
        val trackId = (json \ "trackId").as[String]
        ZapActor.actor ! RequestToUnlockSchedule(trackId, request.webuser)
        Ok("{\"status\":\"success\"}").as("application/json")
      }.getOrElse {
        BadRequest("{\"status\":\"expecting json data\"}").as("application/json")
      }
  }
  
  
}

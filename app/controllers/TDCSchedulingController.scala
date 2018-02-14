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

import library.{SaveTDCSlots, ZapActor}
import models.ConferenceDescriptor.ConferenceProposalTypes
import models._
import org.joda.time.{DateTime, DateTimeZone}
import play.api.i18n.Messages
import play.api.libs.json.{JsNumber, JsString, Json}
import play.api.mvc.Action


/**
  * TDCScheduling Controller.
  * Allows the scheduling of talks for each track in the TDC Conference
  */
object TDCSchedulingController extends SecureCFPController {

  def index = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      val allTracks = ConferenceDescriptor.ConferenceTracks.ALL
        .filter(track => Webuser.hasAccessToAdmin(uuid) | TrackLeader.isTrackLeader(track.id, uuid))
        .sortBy(track => track.label)
      Ok(views.html.Scheduling.scheduling(allTracks))
  }

  def approvedTalks(trackId: String) = SecuredAction(IsMemberOf("cfp")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      import models.Proposal.proposalFormat
      val proposals = ApprovedProposal.allApproved().filter(p => p.track.id == trackId)

      val proposalsWithSpeaker = proposals.map {
        p: Proposal =>
          val mainWebuser = Speaker.findByUUID(p.mainSpeaker)
          val secWebuser = p.secondarySpeaker.flatMap(Speaker.findByUUID)
          val oSpeakers = p.otherSpeakers.map(Speaker.findByUUID)

          // Transform speakerUUID to Speaker name, this simplify Angular Code
          p.copy(
            mainSpeaker = mainWebuser.map(_.cleanName).getOrElse("")
            , secondarySpeaker = secWebuser.map(_.cleanName)
            , otherSpeakers = oSpeakers.flatMap(s => s.map(_.cleanName))
          )

      }

      val json = Json.toJson(
        Map("approvedTalks" -> Json.toJson(
          Map("total" -> JsNumber(proposals.size),
            "talks" -> Json.toJson(proposalsWithSpeaker))
        )
        )
      )
      Ok(Json.stringify(json)).as("application/json")
  }

  def saveSlots(trackId: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      request.body.asJson.map {
        json =>
          ZapActor.actor ! SaveTDCSlots(trackId, json, request.webuser)
          Ok("{\"status\":\"success\"}").as("application/json")
      }.getOrElse {
        BadRequest("{\"status\":\"expecting json data\"}").as("application/json")
      }
  }

  /*
  def slots(trackId: String) = Action {
    implicit request =>
      import Slot.slotFormat

      val tdcConferenceSlots:List[Slot] = {
        val slot1 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T10:10:00.000"),new DateTime("2016-04-22T11:00:00.000"), Room.OTHER)
        val slot2 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T11:10:00.000"),new DateTime("2016-04-22T12:00:00.000"), Room.OTHER)
        val slot3 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T13:10:00.000"),new DateTime("2016-04-22T14:00:00.000"), Room.OTHER)
        val slot4 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T14:10:00.000"),new DateTime("2016-04-22T15:00:00.000"), Room.OTHER)
        val slot5 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T15:40:00.000"),new DateTime("2016-04-22T16:30:00.000"), Room.OTHER)
        val slot6 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T16:40:00.000"),new DateTime("2016-04-22T17:30:00.000"), Room.OTHER)
        val slot7 = SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
          new DateTime("2016-04-22T17:40:00.000"),new DateTime("2016-04-22T18:30:00.000"), Room.OTHER)

        List(slot1,slot2,slot3,slot4,slot5,slot6,slot7)
      }


      val jsSlots = Json.toJson(tdcConferenceSlots)
      Ok(Json.stringify(Json.toJson(Map("allSlots" -> jsSlots)))).as("application/json")
  }


  def allScheduledConfiguration() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      import ScheduleConfiguration.scheduleSavedFormat

      val scheduledSlotsKey = ScheduleConfiguration.allScheduledConfigurationWithLastModified()
      val json = Json.toJson(Map("scheduledConfigurations" -> Json.toJson(
        scheduledSlotsKey.map {
          case (key, dateAsDouble) =>
            val scheduledSaved = Json.parse(key).as[ScheduleSaved]
            Map("key" -> Json.toJson(scheduledSaved),
                "date" -> Json.toJson(new DateTime(dateAsDouble.toLong * 1000).toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
            )
        })
      )
      )
      Ok(Json.stringify(json)).as("application/json")
  }

  def loadScheduledConfiguration(id: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      import ScheduleConfiguration.scheduleConfFormat

      val maybeScheduledConfiguration = ScheduleConfiguration.loadScheduledConfiguration(id)
      maybeScheduledConfiguration match {
        case None => NotFound
        case Some(config) => {
          val configWithSpeakerNames = config.slots.map {
            slot: Slot =>
              slot.proposal match {
                case Some(definedProposal) => {
                  // Create a copy of the proposal, but with clean name
                  val proposalWithSpeakerNames = {
                    val mainWebuser = Speaker.findByUUID(definedProposal.mainSpeaker)
                    val secWebuser = definedProposal.secondarySpeaker.flatMap(Speaker.findByUUID(_))
                    val oSpeakers = definedProposal.otherSpeakers.map(Speaker.findByUUID(_))
                    val preferredDay = Proposal.getPreferredDay(definedProposal.id)

                    val newTitleWithStars: String = s"[${FavoriteTalk.countForProposal(definedProposal.id)}★] ${definedProposal.title}"


                    // Transform speakerUUID to Speaker name, this simplify Angular Code
                    val copiedProposal = definedProposal.copy(
                      title = newTitleWithStars
                      , mainSpeaker = mainWebuser.map(_.cleanName).getOrElse("")
                      , secondarySpeaker = secWebuser.map(_.cleanName)
                      , otherSpeakers = oSpeakers.flatMap(s => s.map(_.cleanName))
                      , privateMessage = preferredDay.getOrElse("")
                    )

                    // Check also if the proposal is still "approved" and not refused
                    // Cause if the talk has been added to schedule, but then refused, we need
                    // to show this as a visual HINT to the admin guy (being Stephan, Antonio or me)
                    if (ApprovedProposal.isApproved(copiedProposal)) {
                      copiedProposal
                    } else {
                      copiedProposal.copy(title = "[Not Approved] " + copiedProposal.title)
                    }

                  }
                  slot.copy(proposal = Option(proposalWithSpeakerNames))
                }
                case None => slot
              }
          }
          Ok(Json.toJson(config.copy(slots = configWithSpeakerNames))).as(JSON)
        }
      }
  }

  def deleteScheduleConfiguration(id: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      ScheduleConfiguration.delete(id)
      Ok("{\"status\":\"deleted\"}").as("application/json")
  }

  def publishScheduleConfiguration() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      request.body.asJson.map {
        json =>
          val id = json.\("id").as[String]
          val confType = json.\("confType").as[String]

          ScheduleConfiguration.publishConf(id, confType)

          Ok("{\"status\":\"success\"}").as("application/json")
      }.getOrElse {
        BadRequest("{\"status\":\"expecting json data\"}").as("application/json")
      }
  }

  def getPublishedSchedule(confType: String, day: Option[String]) = Action {
    implicit request =>
      ScheduleConfiguration.getPublishedSchedule(confType) match {
        case Some(id) => Redirect(routes.Publisher.showAgendaByConfType(confType, Option(id), day.getOrElse("wednesday")))
        case None => Redirect(routes.Publisher.homePublisher).flashing("success" -> Messages("not.published"))
      }
  }

*/
}

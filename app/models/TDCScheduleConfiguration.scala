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

package models

import library.Redis
import play.api.libs.json.{JsValue, Json}

/**
  * Slots that are scheduled.
  *
  */

case class TDCScheduleConfiguration(trackId: String, slots: List[FullTDCSlot], blocked: Option[Boolean] = Some(false))

case class TDCScheduleSaved(createdBy: String, slots: List[TDCSlot], blocked: Option[Boolean] = Some(false))

object TDCScheduleConfiguration {

  implicit val scheduleSavedFormat = Json.format[TDCScheduleSaved]
  implicit val scheduleConfigurationFormat = Json.format[TDCScheduleConfiguration]
  val conferenceId = ConferenceDescriptor.current().eventCode	
  
  def persist(trackId: String, slots: JsValue, createdBy: Webuser): Unit = Redis.pool.withClient {
    implicit client =>
      val tdcScheduleConfiguration = Json.toJson(Map(
        "createdBy" -> Json.toJson(createdBy.cleanName),
        "slots" -> slots))
      val json = tdcScheduleConfiguration.toString()

      client.hset(s"ScheduleConfigurationByTrack:${conferenceId}", trackId, json)
  }

  /**
    * Loads all saved schedules grouped by track
    *
    * @return
    */
  def allScheduledTracks(): Map[String,TDCScheduleSaved] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"ScheduleConfigurationByTrack:${conferenceId}")
            .mapValues(json => Json.parse(json).as[TDCScheduleSaved])
  }

  def delete(trackId: String) = Redis.pool.withClient {
    implicit client =>
      client.hdel(s"ScheduleConfigurationByTrack:${conferenceId}", trackId)
  }

  def loadScheduledConfiguration(trackId: String): Option[TDCScheduleSaved] = Redis.pool.withClient {
    implicit client =>
      client.hget(s"ScheduleConfigurationByTrack:${conferenceId}", trackId).flatMap(json => {
        val maybeScheduledConf = Json.parse(json).validate[TDCScheduleSaved]
        maybeScheduledConf.fold(errors => {
          play.Logger.of("models.TDCScheduleConfiguration").warn("Unable to reload a SlotConfiguration due to JSON error")
          play.Logger.of("models.TDCScheduleConfiguration").warn(s"Got error : ${library.ZapJson.showError(errors)} ")
          None
        }
          , someConf => Option(someConf)
        )
      })
  }

  def deleteAll() = Redis.pool.withClient {
    implicit client =>
      client.del(s"ScheduleConfigurationByTrack:${conferenceId}")
  }

  def updateStatus(trackId:String, status:Boolean) = Redis.pool.withClient {
    implicit client =>
      val updatedSchedule = client.hget(s"ScheduleConfigurationByTrack:${conferenceId}",trackId)
                                  .map(Json.parse(_).as[TDCScheduleSaved].copy(blocked = Some(status)))
      updatedSchedule match {
        case Some(schedule) =>
          client.hset(s"ScheduleConfigurationByTrack:${conferenceId}", trackId, Json.toJson(schedule).toString)
        case None =>
          play.Logger.of("models.TDCScheduleConfiguration").warn(s"Unable to load a SlotConfiguration for the selected track $trackId")
      }
  }
  
  /**
  * returns if a talk is part of a schedule
  */
  def isScheduled(trackId:String, talkId:String):Boolean = {
     val savedSchedule = loadScheduledConfiguration(trackId)
     savedSchedule.map { schedule =>
       !schedule.slots.filter(slot => slot.proposals.contains(talkId)).isEmpty
     }.getOrElse(false)
  }
}
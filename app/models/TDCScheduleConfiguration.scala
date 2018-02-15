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

case class TDCScheduleConfiguration(trackId: String, slots: List[FullTDCSlot])

case class TDCScheduleSaved(createdBy: String, slots: List[TDCSlot])

object TDCScheduleConfiguration {

  implicit val scheduleSavedFormat = Json.format[TDCScheduleSaved]
  implicit val scheduleConfigurationFormat = Json.format[TDCScheduleConfiguration]

  def persist(trackId: String, slots: JsValue, createdBy: Webuser): Unit = Redis.pool.withClient {
    implicit client =>
      val tdcScheduleConfiguration = Json.toJson(Map(
        "createdBy" -> Json.toJson(createdBy.cleanName),
        "slots" -> slots))
      val json = tdcScheduleConfiguration.toString()

      client.hset("ScheduleConfigurationByTrack", trackId, json)
  }

  def allScheduledTracks(): Set[String] = Redis.pool.withClient {
    implicit client =>
      client.hkeys("ScheduleConfigurationByTrack")
  }

  def delete(trackId: String) = Redis.pool.withClient {
    implicit client =>
      client.hdel("ScheduleConfigurationByTrack", trackId)
  }

  def loadScheduledConfiguration(trackId: String): Option[TDCScheduleSaved] = Redis.pool.withClient {
    implicit client =>
      client.hget("ScheduleConfigurationByTrack", trackId).flatMap(json => {
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

}
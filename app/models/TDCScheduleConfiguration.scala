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
import play.api.libs.json.{JsPath, JsValue, Json}
import org.apache.commons.lang3.RandomStringUtils

/**
  * Slots that are scheduled.
  *
  */

object TDCScheduleConfiguration {

  def persist(trackId: String, slots: JsValue, createdBy: Webuser): Unit = Redis.pool.withClient {
    implicit client =>
      val tdcScheduleConfiguration = Json.toJson(Map(
        "createdBy" -> Json.toJson(createdBy.cleanName),
        "slots" -> slots))
      val json = tdcScheduleConfiguration.toString()

      client.hset("ScheduleConfigurationByTrack", trackId, json)
  }

}
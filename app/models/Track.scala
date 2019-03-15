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

import play.api.libs.json.Json
import library.Redis
import play.api.data.Forms._
import play.api.data._
import org.apache.commons.lang3.RandomStringUtils

/**
 * A Track is a general topic (Java, Architecture, Security)
 *
 * Author: nicolas martignole
 * Created: 06/11/2013 01:41
 */
case class Track(id: String, label: String, primaryKey: Option[String] = None)

object Track {
    implicit val trackFormat = Json.format[Track]

    val conferenceId = ConferenceDescriptor.current().eventCode
	
    val UNKNOWN=Track("unknown", "unknown.label")

    val trackForm = Form(
      mapping(
        "trackId" -> nonEmptyText,
        "trackLabel" -> nonEmptyText,
        "primaryKey" -> optional(text)
      ) (createFromForm)(fillForm)
    )

    def createFromForm(trackId:String, label:String, primaryKey: Option[String]) = {
      Track(trackId,label,Option(primaryKey.getOrElse(generateId())))
    }

    def fillForm(track:Track) = {
      Option((track.id,track.label,track.primaryKey))
    }

    def parse(session:String):Track={
      load(session).getOrElse(UNKNOWN)
    }

    def allIDs= allTracks.map(_.id)

    def allAsIdsAndLabels:Seq[(String,String)] = allTracks.map(a=>(a.id,a.label)).toSeq.sorted

    /**
     * Loads all the tracks from the database
     */
    def allTracks():List[Track] = Redis.pool.withClient {
      client =>
        client.hgetAll(s"Tracks:$conferenceId")
              .values
              .map(Json.parse(_).as[Track])
              .toList
    }
    /**
     * loads the specified track from the database
     */
    def load(primaryKey: String): Option[Track] = Redis.pool.withClient {
      client => {
        val optTrack = client.hget(s"Tracks:$conferenceId", primaryKey)
        optTrack.map(Json.parse(_).as[Track])
      }
    }


    /**
     * saves a track in the database, generating the id if needed
     */
    def save(track:Track) = Redis.pool.withClient {
      client => {
        val json = Json.toJson(track).toString()
        client.hset(s"Tracks:$conferenceId", track.primaryKey.get, json)
      }
    }

    private def generateId(): String = Redis.pool.withClient {
      implicit client =>
        val newId = s"T${RandomStringUtils.randomAlphabetic(3).toUpperCase}"
        if (client.hexists(s"Tracks:$conferenceId", newId)) {
          play.Logger.of("Track").warn(s"Track ID collision with $newId")
          generateId()
        } else {
          newId
        }
    }
}

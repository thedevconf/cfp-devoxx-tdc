package models

import library.Redis
import play.api.data.Forms._
import play.api.data._
import org.apache.commons.lang3.RandomStringUtils
import play.api.libs.json.Json

case class TrackArea(id:String, description:String, tracks:List[Track])

object TrackArea {

    implicit val areaFormat = Json.format[TrackArea]

    val conferenceId = ConferenceDescriptor.current().eventCode

    val areaForm = Form(
      mapping(
        "areaId" -> optional(text),
        "description" -> nonEmptyText
      ) (createFromForm)(fillForm)
    )

    def createFromForm(areaId:Option[String], description:String) = {
      TrackArea(areaId.getOrElse(generateId()),description,Nil)
    }

    def fillForm(area:TrackArea) = {
      Option((Option(area.id),area.description))
    }

    private def generateId(): String = Redis.pool.withClient {
      implicit client =>
        val newId = RandomStringUtils.randomAlphabetic(3).toUpperCase
        if (client.hexists(s"TrackAreas:$conferenceId", newId)) {
          play.Logger.of("TrackArea").warn(s"TrackArea ID collision with $newId")
          generateId()
        } else {
          newId
        }
    }
    /**
     * Loads all the track areas from the database
     */
    def allAreas():Iterable[TrackArea] = Redis.pool.withClient {
      client =>
        client.hgetAll(s"TrackAreas:$conferenceId")
              .values
              .map(Json.parse(_).as[TrackArea])
    }

    /**
    * saves a track area in the database
    */
    def save(trackArea:TrackArea) = Redis.pool.withClient {
      client => {
        val json = Json.toJson(trackArea).toString()
        client.hset(s"TrackAreas:$conferenceId", trackArea.id, json)
      }
    }

    /**
    * loads a track area from the database
    */
    def load(areaId:String):Option[TrackArea] = Redis.pool.withClient {
      client => {
        val optArea = client.hget(s"TrackAreas:$conferenceId", areaId)
        optArea.map(Json.parse(_).as[TrackArea])
      }
    }

    /**
    * removes all the track areas for the current event from the database
    */
    def resetAll() = Redis.pool.withClient {
      client => {
        client.del(s"TrackAreas:$conferenceId")
      }
    }

    /**
    * updates the tracks for all the areas
    */
    def updateAllAreas(tracksByArea: Map[String, Seq[String]]) = Redis.pool.withClient{
      client=>
        val tx = client.multi()
        tracksByArea.foreach {
          case (areaId, trackIds) =>
//            Redis.pool.withClient {
//              client =>
				val oldArea = load(areaId).getOrElse(TrackArea(areaId,"",Nil))
				var newTracks:List[Track] = Nil
			    tx.del(s"TrackAreas:${conferenceId}:${areaId}")
                trackIds.filterNot(_ == "no_track").foreach {
                  trackId: String =>
				    newTracks = newTracks :+ Track.parse(trackId)
                }
				val newArea = oldArea.copy(tracks = newTracks)
                val json = Json.toJson(newArea).toString()
                tx.hset(s"TrackAreas:$conferenceId", newArea.id, json)				
//            }
        }
        tx.exec()
      }
}
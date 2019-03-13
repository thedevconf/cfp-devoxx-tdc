package models

import library.Redis
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json

case class TDCConference (
	eventCode:String
)

object TDCConference {

    implicit val conferenceFormat = Json.format[TDCConference]

    val conferenceForm = Form(
      mapping(
        "eventCode" -> nonEmptyText
      ) (createFromForm)(fillForm)
    )

    def createFromForm(eventCode:String) = {
      TDCConference(eventCode)
    }

    def fillForm(conference:TDCConference) = {
      Option((conference.eventCode))
    }

    /**
     * Loads all the conferences from the database
     */
    def allConferences(): List[TDCConference] = Redis.pool.withClient {
      client =>
        client.hgetAll("Conferences")
              .values
              .map(Json.parse(_).as[TDCConference])
              .toList
    }

    /**
     * loads the specified conference from the database
     */
    def load(eventCode:String):Option[TDCConference] = Redis.pool.withClient {
      client => {
        val optConference = client.hget("Conferences", eventCode)
        optConference.map(Json.parse(_).as[TDCConference])
      }
    }

    /**
     * saves a conference in the database
     */
    def save(conference:TDCConference) = Redis.pool.withClient {
      client => {
        val json = Json.toJson(conference).toString()
        client.hset("Conferences", conference.eventCode, json)
      }
    }
}
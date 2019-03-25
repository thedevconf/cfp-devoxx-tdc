package models

import java.util.Date

import library.Redis
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json
import org.joda.time.LocalDate

case class TDCConference (
  eventCode:String,
  title:String,
  shortTitle:String,
  localisation:String,
  cfpOpenDate:LocalDate,
  cfpCloseDate:LocalDate,
  scheduleAnnouncedOn:LocalDate,
  startDate:LocalDate,
  endDate:LocalDate,
  registrationUrl:String,
  scheduleUrl:String
)

object TDCConference {

    implicit val conferenceFormat = Json.format[TDCConference]

    val conferenceForm = Form(
      mapping(
        "eventCode" -> nonEmptyText,
        "conferenceTitle" -> nonEmptyText,
        "conferenceShortTitle" -> nonEmptyText,
        "localisation" -> nonEmptyText,
        "cfpOpenDate" -> jodaLocalDate("dd/MM/yyyy"),
        "cfpCloseDate" -> jodaLocalDate("dd/MM/yyyy"),
        "scheduleAnnouncedOn" -> jodaLocalDate("dd/MM/yyyy"),
        "startDate" -> jodaLocalDate("dd/MM/yyyy"),
        "endDate" -> jodaLocalDate("dd/MM/yyyy"),
        "registrationUrl" -> nonEmptyText,
        "scheduleUrl" -> nonEmptyText
      ) (createFromForm)(fillForm)
    )

    def createFromForm(eventCode:String, title:String, shortTitle:String, localisation:String
                       ,cfpOpenDate:LocalDate, cfpCloseDate:LocalDate, scheduleAnnouncedOn:LocalDate, startDate:LocalDate, endDate:LocalDate
                       , registrationUrl:String, scheduleUrl:String) = {
      TDCConference(eventCode,title, shortTitle, localisation,cfpOpenDate,cfpCloseDate,scheduleAnnouncedOn,startDate,endDate,registrationUrl, scheduleUrl)
    }

    def fillForm(conference:TDCConference) = {
      Option((conference.eventCode,conference.title, conference.shortTitle, conference.localisation
                ,conference.cfpOpenDate,conference.cfpCloseDate,conference.scheduleAnnouncedOn,conference.startDate,conference.endDate
                ,conference.registrationUrl, conference.scheduleUrl))
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
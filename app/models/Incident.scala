package models

import library.Redis
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json

case class IncidentType(id:String, label:String)

object IncidentType {
  implicit val incidentTypeFormat = Json.format[IncidentType]

  val UNKNOWN = IncidentType("invalid","incident.unknown")

  val ALL = List(
    IncidentType("cancel","incident.cancel"),
    IncidentType("noshow","incident.noshow"),
    IncidentType("behavior","incident.behavior")
  )

  def parse(incidentId:String) = {
    ALL.find(t => t.id == incidentId).getOrElse(UNKNOWN)
  }
}

case class Incident(speakerId:String, incidentType:IncidentType, description:String = "", conference:String)

object Incident {

  val incidentForm = Form(
    mapping(
      "speakerId" -> nonEmptyText,
      "incidentType" -> nonEmptyText,
      "description" -> nonEmptyText(maxLength = 1200),
      "conference" -> nonEmptyText
    ) (createFromForm)(fillForm)
  )

  def createFromForm(speakerId:String, incidentId:String, description:String, conference:String) = {
    Incident(speakerId,IncidentType.parse(incidentId),description,conference)
  }

  def fillForm(incident:Incident) = {
    Option((incident.speakerId,incident.incidentType.id,incident.description,incident.conference))
  }

  def save(incident: Incident) = Redis.pool.withClient {
    client =>
      val json = Json.toJson(Map(
        "incidentType" -> Json.toJson(incident.incidentType),
        "description" -> Json.toJson(incident.description),
        "conference" -> Json.toJson(incident.conference)
      )).toString()
      client.sadd("Incidents:" + incident.speakerId, json)
  }

  def allIncidentsByAuthor(uuid:String):List[Incident] = Redis.pool.withClient {
      client =>
        val rawIncidents = client.smembers("Incidents:"+uuid)
        rawIncidents.map(parse(uuid,_)).toList
  }

  def parse(uuid:String, rawIncident:String):Incident = {
    val json = Json.parse(rawIncident)
    Incident(uuid, json.\("incidentType").as[IncidentType],json.\("description").as[String],json.\("conference").as[String])
  }
}

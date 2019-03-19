package library

import models.{Speaker,Event,Proposal}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WS, WSAuthScheme}
import play.api.Play
import play.api.Play.current
import play.api.i18n.Messages

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object TDCClient {


  /**
   * calls the TDC API to update the speaker profile
   */
  def updateSpeakerProfile(speaker: Speaker, caller:Option[String] = None): Unit = {
    val tokenFuture = obtainAPIToken()
    tokenFuture onComplete {
      case Success(token) => {
          val jsonSpeaker = convertSpeakerToJson(speaker)
          val callerId = caller.getOrElse(speaker.uuid)
          callUpdateSpeaker(token,jsonSpeaker,speaker.uuid,callerId)
      }
      case Failure(e) => play.Logger.error(e.getMessage)
    }
  }

  
  /**
   * calls the TDC API to update the talk
   */
  def updateApprovedProposal(proposal: Proposal, caller:Option[String] = None): Unit = {
    val tokenFuture = obtainAPIToken()
    tokenFuture onComplete {
      case Success(token) => {
          val jsonSpeaker = convertProposalToJson(proposal)
          val callerId = caller.getOrElse(proposal.mainSpeaker)
          callUpdateApprovedProposal(token,jsonSpeaker,proposal.id,callerId)
      }
      case Failure(e) => play.Logger.error(e.getMessage)
    }
  }

  private def convertSpeakerToJson(speaker:Speaker):JsValue = {
    Json.toJson(Map(
      "email" -> Json.toJson(speaker.email),
      "nome" -> Json.toJson(speaker.cleanName),
      "nome_social" -> Json.toJson(speaker.tagName),
      "celular" -> Json.toJson(speaker.phone),
      "empresa" -> Json.toJson(speaker.company.getOrElse("")),
      "etnia" -> Json.toJson(speaker.race.getOrElse("")),
      "deficiencia" -> Json.toJson(speaker.disability.getOrElse("")),
      "genero" -> Json.toJson(speaker.gender.getOrElse("")),
      "url_foto" -> Json.toJson(speaker.avatarUrl.getOrElse("")),
      "tamanho_camiseta" -> Json.toJson(speaker.tshirtSize.map("BÁSICA "+_).getOrElse("")),
      "twitter" -> Json.toJson(speaker.twitter.getOrElse("")),
      "facebook" -> Json.toJson(speaker.facebook.getOrElse("")),
      "linkedin" -> Json.toJson(speaker.linkedIn.getOrElse("")),
      "github" -> Json.toJson(speaker.github.getOrElse("")),
      "url_blog" -> Json.toJson(speaker.blog.getOrElse("")),
      "bio" -> Json.toJson(speaker.bio)
    ))
  }

   private def convertProposalToJson(proposal:Proposal):JsValue = {
    Json.toJson(Map(
      "id" -> Json.toJson(proposal.id),
      "titulo" -> Json.toJson(proposal.title),
      "descricao" -> Json.toJson(proposal.summary),
      "urlApresentacao" -> Json.toJson(""),
      "urlDemo" -> Json.toJson(""),
      "urlCodigo" -> Json.toJson("")
    ))
  } 
  
  private def obtainAPIToken():Future[String] = {
    val url = "https://api.globalcode.com.br/v1/oauth2/token"
    val auth:Option[(String,String)] = for (clientId <- Play.current.configuration.getString("tdc.client_id");
                    clientSecret <- Play.current.configuration.getString("tdc.client_secret")) yield (clientId, clientSecret)
    auth.map {
      case (clientId, clientSecret) => {
        val wsCall = WS.url(url).withAuth(clientId, clientSecret,WSAuthScheme.BASIC).get()
        wsCall.map{
          response => {
            response.status match {
               case 200 => (response.json \ "Access-Token").as[String]
               case other => throw new RuntimeException(s"error obtaining token - $other")
             }
          }
        }
      }
    }.getOrElse {
      Future(throw new RuntimeException("tdc.client_secret is not configured in application.conf"))
    }
  }

  private def callUpdateSpeaker(token: String, body: JsValue, speakerId:String, callerId:String):Unit = {
    val url = "https://api.globalcode.com.br/v1/system/palestrante"
    val wsCall = WS.url(url).withHeaders("Authorization" -> s"Bearer $token").put(body)
    wsCall.onComplete {
        case Success(response) => response.status match {
           case 200 => Event.storeEvent(Event(speakerId, callerId, Messages("api.tdc.updatespeaker.success",speakerId)))
           case anotherStatus => {
             play.Logger.error(Messages("api.tdc.updatespeaker.error",speakerId,anotherStatus))
             Event.storeEvent(Event(speakerId, callerId, Messages("api.tdc.updatespeaker.error",speakerId,anotherStatus)))
           }
        }
        case Failure(e) => play.Logger.error(e.getMessage)
    }
  }

  private def callUpdateApprovedProposal(token: String, body: JsValue, proposalId:String, callerId:String):Unit = {
    val url = "https://api.globalcode.com.br/v1/system/palestra"
	println(body)

    val wsCall = WS.url(url).withHeaders("Authorization" -> s"Bearer $token").put(body)
    wsCall.onComplete {
        case Success(response) => response.status match {
           case 200 => Event.storeEvent(Event(proposalId, callerId, Messages("api.tdc.updateproposal.success",proposalId)))
           case anotherStatus => {
             play.Logger.error(Messages("api.tdc.updateproposal.error",proposalId,anotherStatus))
             Event.storeEvent(Event(proposalId, callerId, Messages("api.tdc.updateproposal.error",proposalId,anotherStatus)))
           }
        }
        case Failure(e) => play.Logger.error(e.getMessage)
    }
  }  
  
}
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

import library.Benchmark
import models._
import org.joda.time.{DateTime, DateTimeZone}
import play.api.i18n.Messages
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc.{SimpleResult, _}

import scala.concurrent.{ExecutionContext, Future}

/**
  * A real REST api for men.
  * Created by Nicolas Martignole on 25/02/2014.
  */
object RestAPI extends Controller {

  def index = UserAgentActionAndAllowOrigin {
    implicit request =>
      Ok(views.html.RestAPI.index())
  }

  def profile(docName: String) = Action {
    implicit request =>

      docName match {
        case "link" => Ok(views.html.RestAPI.docLink())
        case "links" => Ok(views.html.RestAPI.docLink())
        case "speaker" => Ok(views.html.RestAPI.docSpeaker())
        case "list-of-speakers" => Ok(views.html.RestAPI.docSpeakers())
        case "all-archived-speakers" => Ok(views.html.RestAPI.docAllArchivedSpeakers())
        case "talk" => Ok(views.html.RestAPI.docTalk())
        case "conference" => Ok(views.html.RestAPI.docConference())
        case "conferences" => Ok(views.html.RestAPI.docConferences())
        case "schedules" => Ok(views.html.RestAPI.docSchedules())
        case "schedule" => Ok(views.html.RestAPI.docSchedule())
        case "proposalType" => Ok(views.html.RestAPI.docProposalType())
        case "tracks" => Ok(views.html.RestAPI.docTrack())
        case "track" => Ok(views.html.RestAPI.docTrack())
        case "room" => Ok(views.html.RestAPI.docRoom())
        case "approved" => Ok(views.html.RestAPI.docApprovedByTrack())
        case "backup" => Ok(views.html.RestAPI.docAllBackup())
        case "all-talks" => Ok(views.html.RestAPI.docAllTalks())
        case "all-archived-talks" => Ok(views.html.RestAPI.docAllArchivedTalks())
        case other => NotFound("Sorry, no documentation for this profile")
      }
  }

  def showAllConferences() = UserAgentActionAndAllowOrigin {
    implicit request =>

      val conferences = Conference.all
      val etag = conferences.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {
          val jsonObject = Json.toJson(
            Map(
              "content" -> Json.toJson("All conferences"),
              "links" -> Json.toJson {
                Conference.all.map {
                  conference: Conference =>
                    conference.link
                }
              }
            )
          )
          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("conferences").absoluteURL().toString + ">; rel=\"profile\""))
        }
      }
  }

  def redirectToConferences = UserAgentActionAndAllowOrigin {
    implicit request =>
      Redirect(routes.RestAPI.showAllConferences())
  }

  def showConference(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      Conference.find(eventCode).map {
        conference: Conference =>

          val etag = conference.eventCode.toString

          request.headers.get(IF_NONE_MATCH) match {
            case Some(tag) if tag == etag => {
              NotModified
            }
            case other => {

              val allProposalTypesIds = ConferenceDescriptor.ConferenceProposalTypes.ALL.map {
                proposalType =>
                  Json.toJson(proposalType.id)
              }

              val jsonObject = Json.toJson(
                Map(
                  "eventCode" -> Json.toJson(conference.eventCode),
                  "label" -> Json.toJson(conference.label),
                  "locale" -> Json.toJson(conference.locale),
                  "localisation" -> Json.toJson(conference.localisation),
                  "days" -> Json.toJson(
                    ConferenceDescriptor.current().timing.days.map(_.toString("EEEE", ConferenceDescriptor.current().locale.head)).toSeq
                  ),
                  "proposalTypesId" -> Json.toJson(allProposalTypesIds),
                  //TODO

                  "links" -> Json.toJson(List(
                    Link(
                      routes.RestAPI.showSpeakers(conference.eventCode).absoluteURL(),
                      routes.RestAPI.profile("list-of-speakers").absoluteURL(),
                      "See all speakers"
                    ),
                    Link(
                      routes.RestAPI.showAllSchedules(conference.eventCode).absoluteURL(),
                      routes.RestAPI.profile("schedules").absoluteURL(),
                      "See the whole agenda"
                    ),
                    Link(
                      routes.RestAPI.showProposalTypes(conference.eventCode).absoluteURL(),
                      routes.RestAPI.profile("proposalType").absoluteURL(),
                      "See the different kind of conferences"
                    ),
                    Link(
                      routes.RestAPI.showTracks(conference.eventCode).absoluteURL(),
                      routes.RestAPI.profile("track").absoluteURL(),
                      "See the different kind of tracks"
                    )
                  ))
                )
              )
              Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
                "Links" -> ("<" + routes.RestAPI.profile("conference").absoluteURL() + ">; rel=\"profile\""))
            }
          }
      }.getOrElse(NotFound("Conference not found"))
  }

  // Load the list of Speakers, from the published Schedule
  def showSpeakers(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      // First load published slots
      val publishedConf = ScheduleConfiguration.loadAllPublishedSlots().filter(_.proposal.isDefined)

      val allSpeakersIDs = publishedConf.flatMap(_.proposal.get.allSpeakerUUIDs).toSet

      val etag = allSpeakersIDs.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {
          val onlySpeakersThatAcceptedTerms: Set[String] = allSpeakersIDs.filterNot(uuid => Speaker.needsToAccept(uuid))
          val speakers = Speaker.loadSpeakersFromSpeakerIDs(onlySpeakersThatAcceptedTerms)

          val updatedSpeakers = speakers.sortBy(_.name).map {
            speaker: Speaker =>
              Map(
                "uuid" -> Json.toJson(speaker.uuid),
                "firstName" -> speaker.firstName.map(Json.toJson(_)).getOrElse(JsNull),
                "lastName" -> speaker.name.map(Json.toJson(_)).getOrElse(JsNull),
                "avatarURL" -> speaker.avatarUrl.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                "twitter" -> speaker.twitter.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                "company" -> speaker.company.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                "links" -> Json.toJson(List(
                  Link(routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL(),
                    routes.RestAPI.profile("speaker").absoluteURL(),
                    speaker.cleanName)
                )
                )
              )
          }

          val jsonObject = Json.toJson(updatedSpeakers)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("list-of-speakers").absoluteURL() + ">; rel=\"profile\"")
          )
        }
      }
  }

  def redirectToSpeakers(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>
      Redirect(routes.RestAPI.showSpeakers(eventCode))
  }

  def showSpeaker(eventCode: String, uuid: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      Speaker.findByUUID(uuid).map {
        speaker =>
          val etag = speaker.hashCode.toString

          request.headers.get(IF_NONE_MATCH) match {
            case Some(tag) if tag == etag => {
              NotModified
            }
            case other => {
              val acceptedProposals = ApprovedProposal.allAcceptedTalksForSpeaker(speaker.uuid)

              val updatedTalks = acceptedProposals.map {
                proposal: Proposal =>
                  val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                    uuid => Speaker.findByUUID(uuid)
                  }.map {
                    speaker =>
                      Link(routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                        routes.RestAPI.profile("speaker").absoluteURL().toString,
                        speaker.cleanName)
                  }

                  Map(
                    "id" -> Json.toJson(proposal.id),
                    "title" -> Json.toJson(proposal.title),
                    "track" -> Json.toJson(Messages(proposal.track.label)),
                    "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                    "links" -> Json.toJson(
                      List(
                        Link(routes.RestAPI.showTalk(eventCode, proposal.id).absoluteURL().toString,
                          routes.RestAPI.profile("talk").absoluteURL().toString, "More details about this talk"
                        )
                      ).++(allSpeakers)
                    )
                  )
              }

              val updatedSpeaker =
                Map(
                  "uuid" -> Json.toJson(speaker.uuid),
                  "firstName" -> speaker.firstName.map(Json.toJson(_)).getOrElse(JsNull),
                  "lastName" -> speaker.name.map(Json.toJson(_)).getOrElse(JsNull),
                  "avatarURL" -> speaker.avatarUrl.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "blog" -> speaker.blog.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "company" -> speaker.company.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "lang" -> speaker.lang.map(u => Json.toJson(u.trim())).getOrElse(Json.toJson("pt")),
                  "bio" -> Json.toJson(speaker.bio),
                  "email" -> Json.toJson(speaker.email),
                  "bioAsHtml" -> Json.toJson(speaker.bioAsHtml),
                  "twitter" -> speaker.cleanTwitter.map(Json.toJson(_)).getOrElse(JsNull),
                  "acceptedTalks" -> Json.toJson(updatedTalks)
                )

              val jsonObject = Json.toJson(updatedSpeaker)
              Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag, "Links" -> ("<" + routes.RestAPI.profile("speaker").absoluteURL().toString + ">; rel=\"profile\""))
            }
          }
      }.getOrElse(NotFound("Speaker not found"))
  }

  def showTalk(eventCode: String, proposalId: String) = UserAgentActionAndAllowOrigin {
    implicit request =>
      Proposal.findById(proposalId).map {
        proposal =>
          val etag = proposal.hashCode.toString

          request.headers.get(IF_NONE_MATCH) match {
            case Some(tag) if tag == etag => {
              NotModified
            }
            case other => {
              val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                uuid => Speaker.findByUUID(uuid)
              }

              val updatedProposal =
                Map(
                  "id" -> Json.toJson(proposal.id),
                  "title" -> Json.toJson(proposal.title),
                  "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                  "lang" -> Json.toJson(proposal.lang),
                  "summary" -> Json.toJson(proposal.summary),
                  "summaryAsHtml" -> Json.toJson(proposal.summaryAsHtml),
                  "track" -> Json.toJson(Messages(proposal.track.label)),
                  "trackId" -> Json.toJson(proposal.track.id),
                  "speakers" -> Json.toJson(allSpeakers.map {
                    speaker =>
                      Map(
                        "link" -> Json.toJson(
                          Link(
                            routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                            routes.RestAPI.profile("speaker").absoluteURL().toString,
                            speaker.cleanName
                          )
                        ),
                        "name" -> Json.toJson(speaker.cleanName)
                      )
                  })
                )
              val jsonObject = Json.toJson(updatedProposal)
              Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag)
            }
          }
      }.getOrElse(NotFound("Proposal not found"))
  }

  def redirectToTalks(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>
      Redirect(routes.RestAPI.showApprovedTalks(eventCode))
  }

  def showScheduleByTrack(eventCode:String, track: String) = UserAgentActionAndAllowOrigin {

    //Loads the schedule for the track. It contains only the proposal IDs
    val schedule: Option[TDCScheduleSaved] = TDCScheduleConfiguration.loadScheduledConfiguration(track)
    schedule.map(saved => {

      //Loads all the proposals for the track that are scheduled
      val scheduledIds = saved.slots.flatMap(slot => slot.proposals)
      val proposals = ApprovedProposal.allApproved().filter(p => p.track.id == track && scheduledIds.contains(p.id))

      //tries to read all the proposals but an error can be generated if the track is invalid
      //because the trackleader has rejected a proposal but has not removed it from the schedule
      try {
        val fullSlots:List[FullTDCSlot] = saved.slots.map(slot => FullTDCSlot(slot.id, slot.stadium, slot.proposals.map(id => proposals.find(_.id == id).get)))
        val result = Map("trilha" -> Json.toJson(track),
                        "slots" -> Json.toJson(fullSlots.map{slot =>
                          Map(
                            "id" -> Json.toJson(slot.id),
                            "stadium" -> Json.toJson(slot.stadium.getOrElse(false)),
                            "palestras" -> Json.toJson(slot.proposals.map{proposal =>
                              Map(
                                "id" -> Json.toJson(proposal.id),
                                "titulo" -> Json.toJson(proposal.title),
                                "tipo" -> Json.toJson(proposal.talkType.label),
                                "descricao" -> Json.toJson(proposal.summaryAsHtml),
                                "status" -> Json.toJson(proposal.state.code),
                                "palestrantes" -> Json.toJson(proposal.allSpeakers.map { speaker =>
                                  Map(
                                    "nome" -> Json.toJson(speaker.cleanName),
                                    "email" -> Json.toJson(speaker.email),
                                    "empresa" -> Json.toJson(speaker.company),
                                    "minibio" -> Json.toJson(speaker.bioAsHtml),
                                    "foto" -> Json.toJson(speaker.avatarUrl),
                                    "blog" -> Json.toJson(speaker.blog),
                                    "twitter" -> Json.toJson(speaker.twitter),
                                    "linkedin" -> Json.toJson(speaker.linkedIn),
                                    "github" -> Json.toJson(speaker.github),
                                    "facebook" -> Json.toJson(speaker.facebook),
                                    "instagram" -> Json.toJson(speaker.instagram),
                                    "telefone" -> Json.toJson(speaker.phone),
                                    "genero" -> Json.toJson(speaker.gender),
                                    "etnia" -> Json.toJson(speaker.race),
                                    "deficiencia" -> Json.toJson(speaker.disability),
                                    "cracha" -> Json.toJson(speaker.tagName),
                                    "camiseta" -> Json.toJson(speaker.tshirtSize)
                                  )
                                }) // end palestrantes
                              )
                            }) //end proposals
                          )
                        })//end slots
        )
        Ok(Json.toJson(result)).as(JSON)
      } catch {
        case e:Exception => {
          val result = Map("reason" -> Json.toJson(e.getClass.getName),
                            "message" -> Json.toJson(Messages("api.loadschedule.error")))
          InternalServerError(Json.toJson(result)).as(JSON)
        }
      }
    }).getOrElse(NotFound(s"Track $track has no schedule"))
  
  }

  def showApprovedTalksByTrack(eventCode: String, track: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val proposalsApproved: List[Proposal] =
        Proposal.allApproved().filter(_.track.id == track)

      val proposalsAccepted: List[Proposal] =
        Proposal.allAccepted().filter(_.track.id == track)

      val proposals = proposalsApproved ::: proposalsAccepted

      val proposalsAndSpeakers: List[(Proposal, List[Speaker])] =
        proposals.map(proposal => (proposal, proposal.allSpeakers))

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val listaJson = proposalsAndSpeakers.map {
            par: (Proposal, List[Speaker]) => {
              val proposal = par._1
              val speakers = par._2
              Map(
                "id" -> Json.toJson(proposal.id),
                "trilha" -> Json.toJson(proposal.track.label),
                "titulo" -> Json.toJson(proposal.title),
                "tipo" -> Json.toJson(proposal.talkType.label),
                "descricao" -> Json.toJson(proposal.summaryAsHtml),
                "palestrantes" -> Json.toJson(speakers.map { speaker =>
                  Map(
                    "nome" -> Json.toJson(speaker.cleanName),
                    "email" -> Json.toJson(speaker.email),
                    "empresa" -> Json.toJson(speaker.company),
                    "minibio" -> Json.toJson(speaker.bioAsHtml),
                    "foto" -> Json.toJson(speaker.avatarUrl),
                    "blog" -> Json.toJson(speaker.blog),
                    "twitter" -> Json.toJson(speaker.twitter),
                    "linkedin" -> Json.toJson(speaker.linkedIn),
                    "github" -> Json.toJson(speaker.github),
                    "facebook" -> Json.toJson(speaker.facebook),
                    "instagram" -> Json.toJson(speaker.instagram),
                    "telefone" -> Json.toJson(speaker.phone),
                    "genero" -> Json.toJson(speaker.gender),
                    "etnia" -> Json.toJson(speaker.race),
                    "deficiencia" -> Json.toJson(speaker.disability),
                    "cracha" -> Json.toJson(speaker.tagName),
                    "camiseta" -> Json.toJson(speaker.tshirtSize)
                  )
                })
              )
            }
          }

          val jsonObject = Json.toJson(listaJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("approved").absoluteURL() + ">; rel=\"profile\""))

        }
      }
  }

  def showAllBackupTalks(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val proposals: List[Proposal] = Proposal.allBackupProposals()

      val proposalsAndSpeakers: List[(Proposal, List[Speaker])] =
        proposals.map(proposal => (proposal, proposal.allSpeakers))

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val listaJson = proposalsAndSpeakers.map {
            par: (Proposal, List[Speaker]) => {
              val proposal = par._1
              val speakers = par._2
              Map(
                "id" -> Json.toJson(proposal.id),
                "trilha" -> Json.toJson(proposal.track.label),
                "titulo" -> Json.toJson(proposal.title),
                "tipo" -> Json.toJson(proposal.talkType.label),
                "descricao" -> Json.toJson(proposal.summaryAsHtml),
                "palestrantes" -> Json.toJson(speakers.map { speaker =>
                  Map(
                    "nome" -> Json.toJson(speaker.cleanName),
                    "email" -> Json.toJson(speaker.email),
                    "empresa" -> Json.toJson(speaker.company),
                    "minibio" -> Json.toJson(speaker.bioAsHtml),
                    "foto" -> Json.toJson(speaker.avatarUrl),
                    "blog" -> Json.toJson(speaker.blog),
                    "twitter" -> Json.toJson(speaker.twitter),
                    "linkedin" -> Json.toJson(speaker.linkedIn),
                    "github" -> Json.toJson(speaker.github),
                    "facebook" -> Json.toJson(speaker.facebook),
                    "instagram" -> Json.toJson(speaker.instagram),
                    "telefone" -> Json.toJson(speaker.phone),
                    "genero" -> Json.toJson(speaker.gender),
                    "etnia" -> Json.toJson(speaker.race),
                    "deficiencia" -> Json.toJson(speaker.disability),
                    "cracha" -> Json.toJson(speaker.tagName),
                    "camiseta" -> Json.toJson(speaker.tshirtSize)
                  )
                })
              )
            }
          }

          val jsonObject = Json.toJson(listaJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("backup").absoluteURL() + ">; rel=\"profile\""))

        }
      }
  }

  def showApprovedTalks(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>
      import models.Proposal.proposalFormat

      // TODO filter on the specified eventCode and not on stupidEventCode when Proposal is updated
      // We cannot right now, as we stored the Proposal with event==Message("longYearlyName") See Proposal.scala in validateNEwProposal
      // So I need to do a temporary filter
      // val proposals = ApprovedProposal.allApproved().filterNot(_.event==eventCode).toList.sortBy(_.title)

      val stupidEventCode = ConferenceDescriptor.current().naming.title // Because the value in the DB for Devoxx BE 2015 is not valid
    val proposals = ApprovedProposal.allApproved().filter(_.event == stupidEventCode).toList.sortBy(_.title)

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val proposalsWithSpeaker = proposals.map {
            p: Proposal =>
              val mainWebuser = Speaker.findByUUID(p.mainSpeaker)
              val secWebuser = p.secondarySpeaker.flatMap(Speaker.findByUUID(_))
              val oSpeakers = p.otherSpeakers.map(Speaker.findByUUID(_))
              val preferredDay = Proposal.getPreferredDay(p.id)

              // Transform speakerUUID to Speaker name, this simplify Angular Code
              p.copy(
                mainSpeaker = mainWebuser.map(_.cleanName).getOrElse("")
                , secondarySpeaker = secWebuser.map(_.cleanName)
                , otherSpeakers = oSpeakers.flatMap(s => s.map(_.cleanName))
                , privateMessage = preferredDay.getOrElse("")
              )

          }

          val finalJson = Map(
            "talks" -> Json.toJson(
              Map(
                "approved" -> Json.toJson(proposalsWithSpeaker.filter(_.state == ProposalState.APPROVED)),
                "accepted" -> Json.toJson(proposalsWithSpeaker.filter(_.state == ProposalState.ACCEPTED))
              )
            )
          )

          val jsonObject = Json.toJson(finalJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("list-of-approved-talks").absoluteURL().toString + ">; rel=\"profile\"")
          )
        }
      }


  }
  def showAllArchivedSpeakers(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val proposals: List[Proposal] = Proposal.allAchivedProposals(eventCode)

      val proposalsAndSpeakers: List[(Proposal, List[Speaker])] =
        proposals.map(proposal => (proposal, proposal.allSpeakers))

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val listaJson = proposalsAndSpeakers.map {
            par: (Proposal, List[Speaker]) => {
              val proposal = par._1
              val speakers = par._2
              Map(
                "id" -> Json.toJson(proposal.id),
                "palestrantes" -> Json.toJson(speakers.map { speaker =>
                  Map(
                    "nome" -> Json.toJson(speaker.cleanName),
                    "email" -> Json.toJson(speaker.email),
                    "empresa" -> Json.toJson(speaker.company),
                    "minibio" -> Json.toJson(speaker.bioAsHtml),
                    "twitter" -> Json.toJson(speaker.twitter),
                    "foto" -> Json.toJson(speaker.avatarUrl),
                    "blog" -> Json.toJson(speaker.blog),
                    "phone" -> Json.toJson(speaker.phone),
                    "gender" -> Json.toJson(speaker.gender),
                    "tshirtSize" -> Json.toJson(speaker.tshirtSize),
                    "linkedIn" -> Json.toJson(speaker.linkedIn),
                    "github" -> Json.toJson(speaker.github),
                    "cracha" -> Json.toJson(speaker.tagName),
                    "facebook" -> Json.toJson(speaker.facebook),
                    "instagram" -> Json.toJson(speaker.instagram),
                    "race" -> Json.toJson(speaker.race),
                    "disability" -> Json.toJson(speaker.disability)
                  )
                })
              )
            }
          }

          val jsonObject = Json.toJson(listaJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("all-archived-speakers").absoluteURL() + ">; rel=\"profile\""))

        }
      }
  }

  def showAllArchivedTalks(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val proposals: List[Proposal] = Proposal.allAchivedProposals(eventCode)

      val proposalsAndSpeakers: List[(Proposal, List[Speaker])] =
        proposals.map(proposal => (proposal, proposal.allSpeakers))

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val listaJson = proposalsAndSpeakers.map {
            par: (Proposal, List[Speaker]) => {
              val proposal = par._1
              val speakers = par._2
              Map(
                "id" -> Json.toJson(proposal.id),
                "trilha" -> Json.toJson(Messages(proposal.track.label)),
                "titulo" -> Json.toJson(proposal.title),
                "tipo" -> Json.toJson(Messages(proposal.talkType.label)),
                "status" -> Json.toJson(Messages(proposal.state.code)),
                "descricao" -> Json.toJson(proposal.summaryAsHtml),
                "palestrantes" -> Json.toJson(speakers.map { speaker =>
                  Map(
                    "nome" -> Json.toJson(speaker.cleanName),
                    "email" -> Json.toJson(speaker.email),
                    "empresa" -> Json.toJson(speaker.company),
                    "minibio" -> Json.toJson(speaker.bioAsHtml),
                    "twitter" -> Json.toJson(speaker.twitter),
                    "foto" -> Json.toJson(speaker.avatarUrl),
                    "blog" -> Json.toJson(speaker.blog),
                    "phone" -> Json.toJson(speaker.phone),
                    "gender" -> Json.toJson(speaker.gender),
                    "tshirtSize" -> Json.toJson(speaker.tshirtSize),
                    "linkedIn" -> Json.toJson(speaker.linkedIn),
                    "github" -> Json.toJson(speaker.github),
                    "cracha" -> Json.toJson(speaker.tagName),
                    "facebook" -> Json.toJson(speaker.facebook),
                    "instagram" -> Json.toJson(speaker.instagram),
                    "race" -> Json.toJson(speaker.race),
                    "disability" -> Json.toJson(speaker.disability)
                  )
                })
              )
            }
          }

          val jsonObject = Json.toJson(listaJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("all-archived-talks").absoluteURL() + ">; rel=\"profile\""))

        }
      }
  }

  def showAllTalks(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val proposals: List[Proposal] = Proposal.allProposals()

      val proposalsAndSpeakers: List[(Proposal, List[Speaker])] =
        proposals.map(proposal => (proposal, proposal.allSpeakers))

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val listaJson = proposalsAndSpeakers.map {
            par: (Proposal, List[Speaker]) => {
              val proposal = par._1
              val speakers = par._2
              Map(
                "id" -> Json.toJson(proposal.id),
                "trilha" -> Json.toJson(Messages(proposal.track.label)),
                "titulo" -> Json.toJson(proposal.title),
                "tipo" -> Json.toJson(Messages(proposal.talkType.label)),
                "status" -> Json.toJson(Messages(proposal.state.code)),
                "descricao" -> Json.toJson(proposal.summaryAsHtml),
                "palestrantes" -> Json.toJson(speakers.map { speaker =>
                  Map(
                    "nome" -> Json.toJson(speaker.cleanName),
                    "email" -> Json.toJson(speaker.email),
                    "empresa" -> Json.toJson(speaker.company),
                    "minibio" -> Json.toJson(speaker.bioAsHtml),
                    "twitter" -> Json.toJson(speaker.twitter),
                    "foto" -> Json.toJson(speaker.avatarUrl),
                    "blog" -> Json.toJson(speaker.blog),
                    "phone" -> Json.toJson(speaker.phone),
                    "gender" -> Json.toJson(speaker.gender),
                    "tshirtSize" -> Json.toJson(speaker.tshirtSize),
                    "linkedIn" -> Json.toJson(speaker.linkedIn),
                    "github" -> Json.toJson(speaker.github),
                    "cracha" -> Json.toJson(speaker.tagName),
                    "facebook" -> Json.toJson(speaker.facebook),
                    "instagram" -> Json.toJson(speaker.instagram),
                    "race" -> Json.toJson(speaker.race),
                    "disability" -> Json.toJson(speaker.disability)
                  )
                })
              )
            }
          }

          val jsonObject = Json.toJson(listaJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("all-talks").absoluteURL() + ">; rel=\"profile\""))

        }
      }
  }

  def showAllTalksPlain(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val proposals: List[Proposal] = Proposal.allProposals()

      val etag = proposals.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val listaJson: List[Map[String, JsValue]] = proposals.map {
            p: Proposal => {
              val mainSpeaker = Speaker.findByUUID(p.mainSpeaker)
              val secSpeaker = p.secondarySpeaker.flatMap(Speaker.findByUUID(_))
              Map(
                "id" -> Json.toJson(p.id),
                "trilha" -> Json.toJson(Messages(p.track.label)),
                "titulo" -> Json.toJson(p.title),
                "tipo" -> Json.toJson(Messages(p.talkType.label)),
                "status" -> Json.toJson(Messages(p.state.code)),
                "descricao" -> Json.toJson(p.summaryAsHtml),

                "nome1" -> mainSpeaker.map(u => Json.toJson(u.cleanName.trim())).getOrElse(Json.toJson("")),
                "email1" -> mainSpeaker.map(u => Json.toJson(u.email)).getOrElse(Json.toJson("")),
                "foto1" -> mainSpeaker.map(u => Json.toJson(u.avatarUrl)).getOrElse(Json.toJson("")),
                "blog1" -> mainSpeaker.map(u => Json.toJson(u.blog)).getOrElse(Json.toJson("")),
                "empresa1" -> mainSpeaker.map(u => Json.toJson(u.company)).getOrElse(Json.toJson("")),
                "lang1" -> mainSpeaker.map(u => Json.toJson(u.lang)).getOrElse(Json.toJson("pt")),
                "minibio1" -> mainSpeaker.map(u => Json.toJson(u.bioAsHtml)).getOrElse(Json.toJson("")),
                "twitter1" -> mainSpeaker.map(u => Json.toJson(u.cleanTwitter)).getOrElse(Json.toJson("")),
                "telefone1" -> mainSpeaker.map(u => Json.toJson(u.phone)).getOrElse(Json.toJson("")),
                "genero1" -> mainSpeaker.map(u => Json.toJson(u.gender)).getOrElse(Json.toJson("")),
                "camiseta1" -> mainSpeaker.map(u => Json.toJson(u.tshirtSize)).getOrElse(Json.toJson("")),

                "nome2" -> secSpeaker.map(u => Json.toJson(u.cleanName.trim())).getOrElse(Json.toJson("")),
                "email2" -> secSpeaker.map(u => Json.toJson(u.email)).getOrElse(Json.toJson("")),
                "foto2" -> secSpeaker.map(u => Json.toJson(u.avatarUrl)).getOrElse(Json.toJson("")),
                "blog2" -> secSpeaker.map(u => Json.toJson(u.blog)).getOrElse(Json.toJson("")),
                "empresa2" -> secSpeaker.map(u => Json.toJson(u.company)).getOrElse(Json.toJson("")),
                "lang2" -> secSpeaker.map(u => Json.toJson(u.lang)).getOrElse(Json.toJson("pt")),
                "minibio2" -> secSpeaker.map(u => Json.toJson(u.bioAsHtml)).getOrElse(Json.toJson("")),
                "twitter2" -> secSpeaker.map(u => Json.toJson(u.cleanTwitter)).getOrElse(Json.toJson("")),
                "telefone2" -> secSpeaker.map(u => Json.toJson(u.phone)).getOrElse(Json.toJson("")),
                "genero2" -> secSpeaker.map(u => Json.toJson(u.gender)).getOrElse(Json.toJson("")),
                "camiseta2" -> secSpeaker.map(u => Json.toJson(u.tshirtSize)).getOrElse(Json.toJson(""))

              )
            }
          }

          val jsonObject = Json.toJson(listaJson)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
            "Links" -> ("<" + routes.RestAPI.profile("all-talks-plain").absoluteURL() + ">; rel=\"profile\""))

        }
      }
  }

  def showAllSchedules(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val mapOfSchedules = Map(
        "links" -> Json.toJson(List(
          //          Link(
          //            routes.RestAPI.showScheduleFor(eventCode, "monday").absoluteURL().toString,
          //            routes.RestAPI.profile("schedule").absoluteURL().toString,
          //            Messages("sw.show.title.mon")
          //          ), Link(
          //            routes.RestAPI.showScheduleFor(eventCode, "tuesday").absoluteURL().toString,
          //            routes.RestAPI.profile("schedule").absoluteURL().toString,
          //            Messages("sw.show.title.tue")
          //          ),
          Link(
            routes.RestAPI.showScheduleFor(eventCode, "wednesday").absoluteURL().toString,
            routes.RestAPI.profile("schedule").absoluteURL().toString,
            Messages("sw.show.title.wed")
          ),
          Link(
            routes.RestAPI.showScheduleFor(eventCode, "thursday").absoluteURL().toString,
            routes.RestAPI.profile("schedule").absoluteURL().toString,
            Messages("sw.show.title.thu")
          ),
          Link(
            routes.RestAPI.showScheduleFor(eventCode, "friday").absoluteURL().toString,
            routes.RestAPI.profile("schedule").absoluteURL().toString,
            Messages("sw.show.title.fri")
          )
        ))
      )
      val newEtag = mapOfSchedules.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == newEtag => NotModified
        case other => {
          val jsonObject = Json.toJson(mapOfSchedules)
          Ok(jsonObject).as(JSON).withHeaders(ETAG -> newEtag, "Links" -> ("<" + routes.RestAPI.profile("schedules").absoluteURL().toString + ">; rel=\"profile\""))
        }
      }
  }

  def showScheduleFor(eventCode: String, day: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val finalListOfSlots = ScheduleConfiguration.getPublishedScheduleByDay(day)
      val newEtag = "v2_" + finalListOfSlots.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == newEtag => NotModified
        case other => {
          val toReturn = finalListOfSlots.map {
            slot =>
              val upProposal = slot.proposal.map {
                proposal =>
                  val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                    uuid => Speaker.findByUUID(uuid)
                  }
                  val updatedProposal =
                    Map(
                      "id" -> Json.toJson(proposal.id),
                      "title" -> Json.toJson(proposal.title),
                      "lang" -> Json.toJson(proposal.lang),
                      "summaryAsHtml" -> Json.toJson(proposal.summaryAsHtml),
                      "summary" -> Json.toJson(proposal.summary),
                      "track" -> Json.toJson(Messages(proposal.track.label)),
                      "trackId" -> Json.toJson(proposal.track.id),
                      "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                      "speakers" -> Json.toJson(allSpeakers.map {
                        speaker =>
                          Map(
                            "link" -> Json.toJson(
                              Link(
                                routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                                routes.RestAPI.profile("speaker").absoluteURL().toString,
                                speaker.cleanName
                              )
                            ),
                            "name" -> Json.toJson(speaker.cleanName)
                          )
                      })
                    )
                  updatedProposal
              }

              val fromDate = new DateTime(slot.from.getMillis).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))
              val slotToDate = new DateTime(slot.to.getMillis).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))

              Map(
                "slotId" -> Json.toJson(slot.id)
                , "day" -> Json.toJson(slot.day)
                , "roomId" -> Json.toJson(slot.room.id)
                , "roomName" -> Json.toJson(slot.room.name)
                , "fromTime" -> Json.toJson(fromDate.toString("HH:mm"))
                , "fromTimeMillis" -> Json.toJson(fromDate.getMillis)
                , "toTime" -> Json.toJson(slotToDate.toString("HH:mm"))
                , "toTimeMillis" -> Json.toJson(slotToDate.getMillis)
                , "talk" -> upProposal.map(Json.toJson(_)).getOrElse(JsNull)
                , "break" -> Json.toJson(slot.break)
                , "roomSetup" -> Json.toJson(slot.room.setup)
                , "roomCapacity" -> Json.toJson(slot.room.capacity)
                , "notAllocated" -> Json.toJson(slot.notAllocated)
              )
          }
          val jsonObject = Json.toJson(
            Map(
              "slots" -> Json.toJson(toReturn)
            )
          )
          Ok(jsonObject).as(JSON).withHeaders(ETAG -> newEtag, "Links" -> ("<" + routes.RestAPI.profile("schedule").absoluteURL().toString + ">; rel=\"profile\""))
        }
      }


  }

  def showProposalTypes(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val allProposalTypes = ConferenceDescriptor.ConferenceProposalTypes.ALL.map {
        proposalType =>
          Json.toJson {
            Map(
              "id" -> Json.toJson(proposalType.id)
              , "description" -> Json.toJson(Messages(proposalType.label))
              , "label" -> Json.toJson(Messages(proposalType.id))
            )
          }
      }
      val etag = allProposalTypes.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == etag => NotModified
        case other => {
          val jsonObject = Json.toJson(
            Map(
              "content" -> Json.toJson("All types of proposal"),
              "proposalTypes" -> Json.toJson(allProposalTypes)
            )
          )

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag, "Links" -> ("<" + routes.RestAPI.profile("proposalType").absoluteURL().toString + ">; rel=\"profile\""))
        }
      }
  }

  def showTracks(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val allTracks = ConferenceDescriptor.ConferenceTracks.ALL.map {
        track =>
          Json.toJson {
            Map(
              "id" -> Json.toJson(track.id)
              , "imgsrc" -> Json.toJson(s"/assets/tdc2016poa/images/icon_${track.id}.png")
              , "title" -> Json.toJson(Messages(s"track.${track.id}.title"))
              , "description" -> Json.toJson(Messages(s"track.${track.id}.desc"))
            )
          }
      }
      val etag = allTracks.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == etag => NotModified
        case other => {
          val jsonObject = Json.toJson(
            Map(
              "content" -> Json.toJson("All tracks"),
              "tracks" -> Json.toJson(allTracks)
            )
          )

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag, "Links" -> ("<" + routes.RestAPI.profile("track").absoluteURL().toString + ">; rel=\"profile\""))
        }
      }
  }

  def showRooms(eventCode: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val allRooms = ConferenceDescriptor.ConferenceRooms.allRooms.map {
        room =>
          Json.toJson {
            Map(
              "id" -> Json.toJson(room.id)
              , "name" -> Json.toJson(room.name)
              , "capacity" -> Json.toJson(room.capacity)
              , "setup" -> Json.toJson(room.setup)
            )
          }
      }
      val etag = allRooms.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == etag => NotModified
        case other => {
          val jsonObject = Json.toJson(
            Map(
              "content" -> Json.toJson("All rooms"),
              "rooms" -> Json.toJson(allRooms)
            )
          )

          Ok(jsonObject).as(JSON).withHeaders(
            ETAG -> etag,
            "Access-Control-Allow-Origin" -> "*",
            "Links" -> ("<" + routes.RestAPI.profile("room").absoluteURL() + ">; rel=\"profile\""))
        }
      }
  }

  def showScheduleForRoom(eventCode: String, room: String, day: String) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val finalListOfSlots = ScheduleConfiguration.getPublishedScheduleByDay(day)
      val newEtag = "v2-" + room.hashCode + finalListOfSlots.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == newEtag => NotModified
        case other => {
          val toReturn = finalListOfSlots.filter(_.room.id == room).map {
            slot =>
              val upProposal = slot.proposal.map {
                proposal =>
                  val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                    uuid => Speaker.findByUUID(uuid)
                  }
                  val updatedProposal =
                    Map(
                      "id" -> Json.toJson(proposal.id),
                      "title" -> Json.toJson(proposal.title),
                      "lang" -> Json.toJson(proposal.lang),
                      "summaryAsHtml" -> Json.toJson(proposal.summaryAsHtml),
                      "summary" -> Json.toJson(proposal.summary),
                      "track" -> Json.toJson(Messages(proposal.track.label)),
                      "trackId" -> Json.toJson(proposal.track.id),
                      "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                      "speakers" -> Json.toJson(allSpeakers.map {
                        speaker =>
                          Map(
                            "link" -> Json.toJson(
                              Link(
                                routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                                routes.RestAPI.profile("speaker").absoluteURL().toString,
                                speaker.cleanName
                              )
                            ),
                            "name" -> Json.toJson(speaker.cleanName)
                          )
                      })
                    )
                  updatedProposal
              }

              val fromDate = new DateTime(slot.from.getMillis).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))
              val slotToDate = new DateTime(slot.to.getMillis).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))

              Map(
                "slotId" -> Json.toJson(slot.id)
                , "day" -> Json.toJson(slot.day)
                , "roomId" -> Json.toJson(slot.room.id)
                , "roomName" -> Json.toJson(slot.room.name)
                , "fromTime" -> Json.toJson(fromDate.toString("HH:mm"))
                , "fromTimeMillis" -> Json.toJson(fromDate.getMillis)
                , "toTime" -> Json.toJson(slotToDate.toString("HH:mm"))
                , "toTimeMillis" -> Json.toJson(slotToDate.getMillis)
                , "talk" -> upProposal.map(Json.toJson(_)).getOrElse(JsNull)
                , "break" -> Json.toJson(slot.break)
                , "roomSetup" -> Json.toJson(slot.room.setup)
                , "roomCapacity" -> Json.toJson(slot.room.capacity)
                , "notAllocated" -> Json.toJson(slot.notAllocated)
              )
          }
          val jsonObject = Json.toJson(
            Map(
              "slots" -> Json.toJson(toReturn)
            )
          )
          Ok(jsonObject).as(JSON).withHeaders(ETAG -> newEtag, "Links" -> ("<" + routes.RestAPI.profile("schedule").absoluteURL().toString + ">; rel=\"profile\""))
        }
      }
  }

  def topFavedTalks(eventCode: String, limit: Int) = UserAgentActionAndAllowOrigin {
    implicit request =>

      val ifNoneMatch = request.headers.get(IF_NONE_MATCH)
      val topFavedTalks = FavoriteTalk.all().toList.sortBy(_._2).reverse.take(limit)
      val newEtag = "t_" + topFavedTalks.hashCode().toString

      ifNoneMatch match {
        case Some(someEtag) if someEtag == newEtag => NotModified
        case other => {
          val toReturn = topFavedTalks.map {
            case (proposal, vote) =>

              val updatedProposalWithLink = {
                val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                  uuid => Speaker.findByUUID(uuid)
                }.map {
                  speaker =>
                    Link(routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                      routes.RestAPI.profile("speaker").absoluteURL().toString,
                      speaker.cleanName)
                }

                Map(
                  "id" -> Json.toJson(proposal.id),
                  "title" -> Json.toJson(proposal.title),
                  "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                  "talkTypeId" -> Json.toJson(proposal.talkType.id),
                  "links" -> Json.toJson(
                    List(
                      Link(routes.RestAPI.showTalk(eventCode, proposal.id).absoluteURL().toString,
                        routes.RestAPI.profile("talk").absoluteURL().toString, "More details about this talk"
                      )
                    ).++(allSpeakers)
                  )
                )
              }

              val maybeSlot = {
                ScheduleConfiguration.findSlotForConfType(proposal.talkType.id, proposal.id).map {
                  slot =>
                    val fromDate = new DateTime(slot.from.getMillis).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))
                    val slotToDate = new DateTime(slot.to.getMillis).toDateTime(DateTimeZone.forID("America/Sao_Paulo"))

                    Map(
                      "slotId" -> Json.toJson(slot.id)
                      , "day" -> Json.toJson(slot.day)
                      , "roomId" -> Json.toJson(slot.room.id)
                      , "roomName" -> Json.toJson(slot.room.name)
                      , "fromTime" -> Json.toJson(fromDate.toString("HH:mm"))
                      , "fromTimeMillis" -> Json.toJson(fromDate.getMillis)
                      , "toTime" -> Json.toJson(slotToDate.toString("HH:mm"))
                      , "toTimeMillis" -> Json.toJson(slotToDate.getMillis)
                      , "talk" -> Json.toJson(updatedProposalWithLink)
                      , "break" -> Json.toJson(slot.break)
                      , "roomSetup" -> Json.toJson(slot.room.setup)
                      , "roomCapacity" -> Json.toJson(slot.room.capacity)
                      , "notAllocated" -> Json.toJson(slot.notAllocated)
                    )
                }
              }

              Map(
                "vote" -> Json.toJson(vote),
                "slot" -> maybeSlot.map(Json.toJson(_)).getOrElse(JsNull)
              )
          }
          val jsonObject = Json.toJson(
            Map(
              "topTalks" -> Json.toJson(toReturn)
            )
          )
          Ok(jsonObject).as(JSON).withHeaders(ETAG -> newEtag)
        }
      }
  }
}

object UserAgentActionAndAllowOrigin extends ActionBuilder[Request] with play.api.http.HeaderNames {

  import ExecutionContext.Implicits.global

  override def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
    request.headers.get(USER_AGENT).collect {
      case some => {
        block(request).map { result =>
          request.headers.get("Origin") match {
            case Some(o) => result.withHeaders("Access-Control-Allow-Origin" -> o,
              "Access-Control-Expose-Headers" -> "etag,links",
              "Access-Control-Allow-Credentials" -> "true",
              "Access-Control-Max-Age" -> "3600")
            case None => result.withHeaders("X-No-Access" -> "no-origin")
          }
        }
      }
    }.getOrElse {
      Future.successful(play.api.mvc.Results.Forbidden("User-Agent is required to interact with " + ConferenceDescriptor.current().naming.shortTitle + " CFP API"))
    }

  }
}

case class Link(href: String, rel: String, title: String)

object Link {
  implicit val linkFormat = Json.format[Link]
}

case class Conference(eventCode: String, label: String, locale: List[String], localisation: String, link: Link)

object Conference {

  implicit val confFormat = Json.format[Conference]

  def currentConference(implicit req: RequestHeader) = Conference(
    ConferenceDescriptor.current().eventCode,
    ConferenceDescriptor.current().naming.title + ","+ Messages("CONF.dates",ConferenceDescriptor.current().startDate, ConferenceDescriptor.current.endDate),
    ConferenceDescriptor.current().locale.map(_.toString),
    ConferenceDescriptor.current().localisation,
    Link(
      routes.RestAPI.showConference(ConferenceDescriptor.current().eventCode).absoluteURL(),
      routes.RestAPI.profile("conference").absoluteURL(),
      "See more details about " + ConferenceDescriptor.current().naming.title
    ))

  def all(implicit req: RequestHeader) = {
    List(currentConference)
  }

  // Super fast, super crade, super je m'en fiche pour l'instant
  def find(eventCode: String)(implicit req: RequestHeader): Option[Conference] = Option(currentConference)

}
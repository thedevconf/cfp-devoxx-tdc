package controllers

import library.search.ElasticSearch
import library.{ComputeLeaderboard, ComputeVotesAndScore, SendMessageInternal, SendMessageToSpeaker, _}
import models.Review._
import models._
import models.SocialMedia._
import org.apache.commons.lang3.StringUtils
import play.api.data.Forms._
import play.api.data._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}
import models.Incident._

/**
  * The backoffice controller for the CFP technical committee.
  *
  * Author: @nmartignole
  * Created: 11/11/2013 09:09 in Thalys, heading to Devoxx2013
  */
object CFPAdmin extends SecureCFPController {

  def index(sort: Option[String], ascdesc: Option[String], track: Option[String]) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      val sorter = proposalSorter(sort)
      val orderer = proposalOrder(ascdesc)
      val allNotReviewed = Review.allProposalsNotReviewed(uuid)
      val maybeFilteredProposals = track match {
        case None => allNotReviewed
        case Some(trackLabel) => allNotReviewed.filter(_.track.id.equalsIgnoreCase(StringUtils.trimToEmpty(trackLabel)))
      }
      val allProposalsForReview = sortProposals(maybeFilteredProposals, sorter, orderer)

      val etag = allProposalsForReview.hashCode().toString

      request.headers.get("If-None-Match") match {
        case Some(tag) if tag == etag => NotModified
        case _ => Ok(views.html.CFPAdmin.cfpAdminIndex(allProposalsForReview, sort, ascdesc)).withHeaders("ETag" -> etag)
      }
  }

  def sortProposals(ps: List[Proposal], sorter: Option[Proposal => String], orderer: Ordering[String]) =
    sorter match {
      case None => ps
      case Some(s) => ps.sortBy(s)(orderer)
    }

  def proposalSorter(sort: Option[String]): Option[Proposal => String] = {
    sort match {
      case Some("title") => Some(_.title)
      case Some("mainSpeaker") => Some(_.mainSpeaker)
      case Some("track") => Some(_.track.label)
      case Some("talkType") => Some(_.talkType.label)
      case _ => None
    }
  }

  def proposalOrder(ascdesc: Option[String]) = ascdesc match {
    case Some("desc") => Ordering[String].reverse
    case _ => Ordering[String]
  }

  val messageForm: Form[String] = Form("msg" -> nonEmptyText(maxLength = 1000))

  def openForReview(proposalId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(proposal) => {
          if(Webuser.hasAccessToAdmin(uuid) | TrackLeader.isTrackLeader(proposal.track.id,uuid)) {
            val speakerDiscussion = Comment.allSpeakerComments(proposal.id)
            val internalDiscussion = Comment.allInternalComments(proposal.id)
            val maybeMyVote = Review.lastVoteByUserForOneProposal(uuid, proposalId)
            val proposalsByAuths = allProposalByProposal(proposal)
            Ok(views.html.CFPAdmin.showProposal(proposal, proposalsByAuths, speakerDiscussion, internalDiscussion, messageForm, messageForm, voteForm, maybeMyVote, uuid))
          } else {
            Redirect(routes.Application.index()).flashing("error" -> "Not Authorized")
          }
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }

  /**
   * Shows a view for an archived proposal
  */ 
  def openArchivedProposal(proposalId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findArchivedById(proposalId) match {
        case Some(proposal) => {
          if(Webuser.hasAccessToAdmin(uuid) | TrackLeader.isTrackLeader(proposal.track.id,uuid)) {
            val speakerDiscussion = Nil
            val internalDiscussion = Nil
            val maybeMyVote = None
            val proposalsByAuths = allProposalByProposal(proposal)
            Ok(views.html.CFPAdmin.showProposal(proposal, proposalsByAuths, speakerDiscussion, internalDiscussion, messageForm, messageForm, voteForm, maybeMyVote, uuid))
          } else {
            Redirect(routes.Application.index()).flashing("error" -> "Not Authorized")
          }
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }
  
  private def allProposalByProposal(proposal: Proposal): Map[String, Map[String, models.Proposal]] = {
    val authorIds: List[String] = proposal.mainSpeaker :: proposal.secondarySpeaker.toList ::: proposal.otherSpeakers
    authorIds.map {
      case id => id -> Proposal.allProposalsByAuthor(id)
    }.toMap

  }

  def showVotesForProposal(proposalId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))).async {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val uuid = request.webuser.uuid
      scala.concurrent.Future {
        Proposal.findById(proposalId) match {
          case Some(proposal) => {
            if(isAuthorizedToVote(uuid,proposal)) {
              val currentAverageScore = Review.averageScore(proposalId)
              val countVotesCast = Review.totalVoteCastFor(proposalId) // votes exprimes (sans les votes a zero)
              val countVotes = Review.totalVoteFor(proposalId)
              val allVotes = Review.allVotesFor(proposalId)

              // The next proposal I should review
              val allNotReviewed = Review.allProposalsNotReviewed(uuid)
              val (sameTracks, otherTracks) = allNotReviewed.partition(_.track.id == proposal.track.id)
              val (sameTalkType, otherTalksType) = allNotReviewed.partition(_.talkType.id == proposal.talkType.id)

              val nextToBeReviewedSameTrack = (sameTracks.sortBy(_.talkType.id) ++ otherTracks).headOption
              val nextToBeReviewedSameFormat = (sameTalkType.sortBy(_.track.id) ++ otherTalksType).headOption

              // If Golden Ticket is active
              if (ConferenceDescriptor.isGoldenTicketActive) {
                val averageScoreGT = ReviewByGoldenTicket.averageScore(proposalId)
                val countVotesCastGT: Option[Long] = Option(ReviewByGoldenTicket.totalVoteCastFor(proposalId))


                Ok(views.html.CFPAdmin.showVotesForProposal(uuid, proposal, currentAverageScore, countVotesCast, countVotes, allVotes, nextToBeReviewedSameTrack, nextToBeReviewedSameFormat, averageScoreGT, countVotesCastGT))
              } else {
                Ok(views.html.CFPAdmin.showVotesForProposal(uuid, proposal, currentAverageScore, countVotesCast, countVotes, allVotes, nextToBeReviewedSameTrack, nextToBeReviewedSameFormat, 0, None))
              }
            } else {
              Redirect(routes.Application.index()).flashing("error" -> "Not Authorized")
            }
          }
          case None => NotFound("Proposal not found").as("text/html")
        }
      }
  }

  def sendMessageToSpeaker(proposalId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(proposal) => {
          messageForm.bindFromRequest.fold(
            hasErrors => {
              val speakerDiscussion = Comment.allSpeakerComments(proposal.id)
              val internalDiscussion = Comment.allInternalComments(proposal.id)
              val maybeMyVote = Review.lastVoteByUserForOneProposal(uuid, proposalId)
              val proposals = allProposalByProposal(proposal)
              BadRequest(views.html.CFPAdmin.showProposal(proposal, proposals, speakerDiscussion, internalDiscussion, hasErrors, messageForm, voteForm, maybeMyVote, uuid))
            },
            validMsg => {
              Comment.saveCommentForSpeaker(proposal.id, uuid, validMsg) // Save here so that it appears immediatly
              ZapActor.actor ! SendMessageToSpeaker(uuid, proposal, validMsg)
              Redirect(routes.CFPAdmin.openForReview(proposalId)).flashing("success" -> "Message sent to speaker.")
            }
          )
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }

  // Post an internal message that is visible only for program committe
  def postInternalMessage(proposalId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(proposal) => {
          messageForm.bindFromRequest.fold(
            hasErrors => {
              val speakerDiscussion = Comment.allSpeakerComments(proposal.id)
              val internalDiscussion = Comment.allInternalComments(proposal.id)
              val maybeMyVote = Review.lastVoteByUserForOneProposal(uuid, proposalId)
              val proposals = allProposalByProposal(proposal)
              BadRequest(views.html.CFPAdmin.showProposal(proposal, proposals, speakerDiscussion, internalDiscussion, messageForm, hasErrors, voteForm, maybeMyVote, uuid))
            },
            validMsg => {
              Comment.saveInternalComment(proposal.id, uuid, validMsg) // Save here so that it appears immediatly
              ZapActor.actor ! SendMessageInternal(uuid, proposal, validMsg)
              Redirect(routes.CFPAdmin.openForReview(proposalId)).flashing("success" -> "Message sent to program committee.")
            }
          )
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }

  val voteForm: Form[Int] = Form("vote" -> number(min = 0, max = 10))

  def voteForProposal(proposalId: String) = SecuredAction(IsMemberOf("cfp")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(proposal) =>
          voteForm.bindFromRequest.fold(
            hasErrors => {
              val speakerDiscussion = Comment.allSpeakerComments(proposal.id)
              val internalDiscussion = Comment.allInternalComments(proposal.id)
              val maybeMyVote = Review.lastVoteByUserForOneProposal(uuid, proposalId)
              val proposals = allProposalByProposal(proposal)
              BadRequest(views.html.CFPAdmin.showProposal(proposal, proposals, speakerDiscussion, internalDiscussion, messageForm, messageForm, hasErrors, maybeMyVote, uuid))
            },
            validVote => {
              if(isAuthorizedToVote(uuid,proposal)) {
                Review.voteForProposal(proposalId, uuid, validVote)
                Redirect(routes.CFPAdmin.showVotesForProposal(proposalId)).flashing("vote" -> "Ok, vote submitted")
              } else {
                Redirect(routes.Application.index()).flashing("error" -> "Not Authorized")
              }
            }
          )
        case None => NotFound("Proposal not found").as("text/html")
      }
  }

  /**
    *
    * Validates if the user has the authorization to vote for a proposal
    *
    * @param uuid identifier of the user that is trying to vote for the proposal
    * @param proposal proposal to receive the vote
    * @return true if the user is authorized to vote for the requested proposal
    */
  private def isAuthorizedToVote(uuid:String, proposal:Proposal) = {
      Webuser.hasAccessToAdmin(uuid) || TrackLeader.isTrackLeader(proposal.track.id,uuid)
  }

  def clearVoteForProposal(proposalId: String) = SecuredAction(IsMemberOf("cfp")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(proposal) => {
          Review.removeVoteForProposal(proposalId, uuid)
          Redirect(routes.CFPAdmin.showVotesForProposal(proposalId)).flashing("vote" -> "Removed your vote")
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }

  def leaderBoard = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      val totalSpeakers = Leaderboard.totalSpeakers()
      val totalProposals = Leaderboard.totalProposals()
      val totalReviews = Leaderboard.totalVotes()
      val totalWithVotes = Leaderboard.totalWithVotes()
      val totalNoVotes = Leaderboard.totalNoVotes()
      val maybeMostVoted = Leaderboard.mostReviewed()
 //     val bestReviewer = Leaderboard.bestReviewer()
 //     val lazyOnes = Leaderboard.lazyOnes()

      val totalSubmittedByTrack = Leaderboard.totalSubmittedByTrack()
      val totalAcceptedByTrack = Leaderboard.totalAcceptedByTrack()

      val totalApprovedSpeakers = Leaderboard.totalApprovedSpeakers()
      val totalRefusedSpeakers = Leaderboard.totalRefusedSpeakers()

      val totalApprovedByTrack:Map[String,Int] = Leaderboard.totalApprovedByTrack()

      val totalByState:List[(String,Int)] = Leaderboard.totalByState()

      val allProposalStatesByTrack:Map[String,LeaderboardProposalStates] = Leaderboard.allProposalStatesByTrack()

      Ok(
        views.html.CFPAdmin.leaderBoard(
          totalSpeakers, totalProposals, totalReviews, totalWithVotes,
          totalNoVotes, maybeMostVoted,
          totalSubmittedByTrack,
          totalAcceptedByTrack,
          totalApprovedSpeakers,
          totalRefusedSpeakers,
          totalApprovedByTrack,
          totalByState,
          allProposalStatesByTrack
        )
      )
  }

  def allReviewersAndStats = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      Ok(views.html.CFPAdmin.allReviewersAndStats(Review.allReviewersAndStats()))
  }

  def doComputeLeaderBoard() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      library.ZapActor.actor ! ComputeLeaderboard()
      Redirect(routes.CFPAdmin.index()).flashing("success" -> Messages("leaderboard.compute"))
  }

  def allMyVotes(talkType: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      ConferenceDescriptor.ConferenceProposalTypes.ALL.find(_.id == talkType).map {
        pType =>
          val uuid = request.webuser.uuid
          val allMyVotes = Review.allVotesFromUser(uuid)
          val allProposalIDs = allMyVotes.map(_._1)

          //reads all proposals for admin users otherwise only the proposals in tracks the user is leader
          val allProposalsForProposalType =
            if(Webuser.hasAccessToAdmin(uuid))
              Proposal.loadAndParseProposals(allProposalIDs).filter(_._2.talkType == pType)
            else
              Proposal.loadAndParseProposals(allProposalIDs).filter(_._2.talkType == pType)
                .filter(tupla => TrackLeader.isTrackLeader(tupla._2.track.id,uuid))

          val allProposalsIdsProposalType = allProposalsForProposalType.keySet

          val allMyVotesForSpecificProposalType = allMyVotes.filter{
            proposalIdAndVotes => allProposalsIdsProposalType.contains(proposalIdAndVotes._1)
          }

          val allScoresForProposals:Map[String,Double] = allProposalsIdsProposalType.map{
            pid:String=>(pid,Review.averageScore(pid))
          }.toMap

          val sortedListOfProposals = allMyVotesForSpecificProposalType.toList.sortBy{
            case(proposalID,maybeScore)=>
              maybeScore.getOrElse(0.toDouble)
          }.reverse

          Ok(views.html.CFPAdmin.allMyVotes(sortedListOfProposals,allProposalsForProposalType,talkType, allScoresForProposals))
      }.getOrElse {
        BadRequest("Invalid proposal type")
      }
  }

  def advancedSearch(q: Option[String] = None, p: Option[Int] = None) = SecuredAction(IsMemberOf("admin")).async {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      import play.api.libs.concurrent.Execution.Implicits.defaultContext

      ElasticSearch.doAdvancedSearch("speakers,proposals", q, p).map {
        case r if r.isSuccess => {
          val json = Json.parse(r.get)
          val total = (json \ "hits" \ "total").as[Int]
          val hitContents = (json \ "hits" \ "hits").as[List[JsObject]]

          val results = hitContents.sortBy {
            jsvalue =>
              val index = (jsvalue \ "_index").as[String]
              index
          }.map {
            jsvalue =>
              val index = (jsvalue \ "_index").as[String]
              val source = (jsvalue \ "_source")
              index match {
                case "proposals" => {
                  val id = (source \ "id").as[String]
                  val title = (source \ "title").as[String]
                  val talkType = Messages((source \ "talkType" \ "id").as[String])
                  val code = (source \ "state" \ "code").as[String]
                  val mainSpeaker = (source \ "mainSpeaker").as[String]
                  s"<p class='searchProposalResult'><i class='icon-folder-open'></i> Proposal <a href='${routes.CFPAdmin.openForReview(id)}'>$title</a> <strong>$code</strong> - by $mainSpeaker - $talkType</p>"
                }
                case "speakers" => {
                  val uuid = (source \ "uuid").as[String]
                  val name = (source \ "name").as[String]
                  val firstName = (source \ "firstName").as[String]
                  s"<p class='searchSpeakerResult'><i class='icon-user'></i> Speaker <a href='${routes.CFPAdmin.showSpeakerAndTalks(uuid)}'>$firstName $name</a></p>"
                }
                case other => "Unknown format " + index
              }
          }

          Ok(views.html.CFPAdmin.renderSearchResult(total, results, q, p)).as("text/html")
        }
        case r if r.isFailure => {
          InternalServerError(r.get)
        }
      }

  }

  def allSponsorTalks = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      val proposals = Proposal.allSponsorsTalk().toList.sortBy(_.title)
      Ok(views.html.CFPAdmin.allSponsorTalks(proposals))
  }

  def showSpeakerAndTalks(uuidSpeaker: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      Speaker.findByUUID(uuidSpeaker) match {
        case Some(speaker) => {
          val proposals = Proposal.allProposalsByAuthor(speaker.uuid)
          val incidents = Incident.allIncidentsByAuthor(speaker.uuid)
          Ok(views.html.CFPAdmin.showSpeakerAndTalks(speaker, proposals, incidents, request.webuser.uuid))
        }
        case None => NotFound("Speaker not found")
      }
  }

  def allVotes(confType: String, track: Option[String]) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      val requesterUUID = request.webuser.uuid

      val reviews: Map[String, (Score, TotalVoter, TotalAbst, AverageNote, StandardDev)] = Review.allVotes()

      //reads all proposals for admin users otherwise only the proposals in tracks the user is leader
      val allProposals =
        if(Webuser.hasAccessToAdmin(requesterUUID))
          Proposal.loadAndParseAllEventProposals(reviews.keySet)
        else
          Proposal.loadAndParseAllEventProposals(reviews.keySet)
                  .filter(tupla => TrackLeader.isTrackLeader(tupla._2.track.id,requesterUUID))

      val notifiedBackups = Event.notifiedBackupProposals()
      val listOfProposals = reviews.flatMap {
        case (proposalId, scoreAndVotes) =>
          val maybeProposal = allProposals.get(proposalId)
          maybeProposal match {
            case None => //play.Logger.of("CFPAdmin").error(s"Unable to load proposal id $proposalId")
              None
            case Some(p) => {
              val goldenTicketScore:Double = ReviewByGoldenTicket.averageScore(p.id)
              val gtVoteCast:Long = ReviewByGoldenTicket.totalVoteCastFor(p.id)
              Option(p, scoreAndVotes, goldenTicketScore, gtVoteCast, notifiedBackups.contains(p.id))
            }
          }
      }

      val tempListToDisplay = confType match {
        case "all" => listOfProposals
        case filterType => listOfProposals.filter(_._1.talkType.id == filterType)
      }

      val listToDisplay = track match {
          case None => tempListToDisplay
          case Some(trackId) => tempListToDisplay.filter(_._1.track.id == trackId)
        }

      Ok(views.html.CFPAdmin.allVotes(listToDisplay.toList))
  }

  def doComputeVotesTotal() = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      ZapActor.actor ! ComputeVotesAndScore()
      Redirect(routes.CFPAdmin.allVotes("all", None)).flashing("success" -> "Recomputing votes and scores...")
  }

  def removeSponsorTalkFlag(proposalId: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.removeSponsorTalkFlag(uuid, proposalId)
      Redirect(routes.CFPAdmin.allSponsorTalks()).flashing("success" -> s"Removed sponsor talk on $proposalId")
  }

  def addSponsorTalkFlag(proposalId: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.addSponsorTalkFlag(uuid, proposalId)
      Redirect(routes.CFPAdmin.allSponsorTalks()).flashing("success" -> s"Added sponsor talk on $proposalId")
  }

  /**
    * Loads a proposal for an administrator to edit
    *
    * @param proposalId
    * @return
    */
  def editProposal(proposalId: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request =>
      val maybeProposal = Proposal.findById(proposalId)
      maybeProposal match {
        case Some(proposal) => {
            val proposalForm = Proposal.proposalForm.fill(proposal)
            Ok(views.html.CFPAdmin.editProposal(proposalForm))
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }

  /**
    * Allows an admin to update the proposal
    *
    * @param proposalId
    * @return
    */
  def updateProposal(proposalId:String) = SecuredAction(IsMemberOf("admin")) {
    implicit request =>
      val uuid = request.webuser.uuid

      Proposal.proposalForm.bindFromRequest.fold(
        hasErrors => BadRequest(views.html.CFPAdmin.editProposal(hasErrors)),
        proposal => {
          Proposal.findById(proposal.id) match {
            case Some(existingProposal) => {
              val updatedProposal = proposal.copy(mainSpeaker = existingProposal.mainSpeaker, secondarySpeaker = existingProposal.secondarySpeaker, otherSpeakers = existingProposal.otherSpeakers)
              Proposal.save(updatedProposal.mainSpeaker, updatedProposal, existingProposal.state)
              Event.storeEvent(Event(proposal.id, uuid, "Edited proposal " + proposal.id + " with current state [" + existingProposal.state.code + "]"))

              // Notifies the TDC application if the state of the proposal is approved or accepted to keep the TDC site in sync with the cfp
              if(existingProposal.state == ProposalState.APPROVED || existingProposal.state == ProposalState.ACCEPTED) {
                ZapActor.actor ! ApprovedProposalUpdated(updatedProposal)
              }
  
              Redirect(routes.CallForPaper.homeForSpeaker()).flashing("success" -> Messages("saved2"))
            }
            case None => NotFound("Proposal not found").as("text/html")
          }
        }
      )
  }

  /**
    *
    * lists all proposals by track if the user is admin
    * otherwise lists the proposals if the user is trackleader of the requested track
    *
    * @param track
    * @return
    */
  def allProposalsByTrack(track: Option[String]) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      val proposals =
        if(Webuser.hasAccessToAdmin(uuid) | TrackLeader.isTrackLeader(track.getOrElse(""),uuid))
          Proposal.allSubmitted().filter(_.track.id == track.getOrElse(""))
        else
          Nil
      Ok(views.html.CFPAdmin.allProposalsByTrack(proposals, track.getOrElse("")))
  }

  def allProposalsByType(confType: String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val proposals = Proposal.allSubmitted().filter(_.talkType.id == confType)
      Ok(views.html.CFPAdmin.allProposalsByType(proposals, confType))
  }

  def showProposalsNotReviewedCompareTo(maybeReviewer: Option[String]) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      maybeReviewer match {
        case None =>
          Ok(views.html.CFPAdmin.selectAnotherWebuser(Webuser.allCFPWebusers()))
        case Some(otherReviewer) =>
          val diffProposalIDs = Review.diffReviewBetween(otherReviewer, uuid)
          Ok(views.html.CFPAdmin.showProposalsNotReviewedCompareTo(diffProposalIDs, otherReviewer))
      }
  }

  def reportsHome() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.CFPAdmin.reportsHome())
  }

  def allTalksByCompany() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      val allProposals = Proposal.allActiveProposals()

      val allSpeakers: Set[Speaker] = allProposals.flatMap(p => p.allSpeakers).toSet
      val groupedSpeakers = allSpeakers
        .groupBy(_.company.map(_.toLowerCase.trim).getOrElse(Messages("reports.talks.nocompany")))
        .toList

      val groupedProposals = groupedSpeakers.map {
        case (company, speakers) =>
          val setOfProposals = speakers.flatMap {
            s =>
              allProposals.filter(_.allSpeakerUUIDs.contains(s.uuid))
          }
          (company, setOfProposals)
      }

      Ok(views.html.CFPAdmin.allTalksByCompany(groupedProposals))
  }

  def allSpeakersWithApprovedTalks() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val allSpeakers = ApprovedProposal.allApprovedSpeakers()
      Ok(views.html.CFPAdmin.allSpeakers(allSpeakers.toList.sortBy(_.cleanName)))
  }

  def allApprovedSpeakersByCompany(showQuickiesAndBof:Boolean) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val speakers = ApprovedProposal.allApprovedSpeakers()
        .groupBy(_.company.map(_.toLowerCase.trim).getOrElse(""))
        .toList
        .sortBy(_._2.size)
        .reverse

      val proposals = speakers.map {
        case (company, subSpeakers) =>
          val setOfProposals = subSpeakers.toList.flatMap {
            s =>
              Proposal.allApprovedProposalsByAuthor(s.uuid).values
          }.toSet.filterNot { p: Proposal =>
  //            if(showQuickiesAndBof){
                p==null
  //            }else{
  //              p.talkType==ConferenceDescriptor.ConferenceProposalTypes.BOF ||
  //              p.talkType==ConferenceDescriptor.ConferenceProposalTypes.QUICK
  //            }
          }
          (company, setOfProposals)
      }

      Ok(views.html.CFPAdmin.allApprovedSpeakersByCompany(speakers, proposals))
  }

  // All speakers that accepted to present a talk (including BOF and Quickies)
  def allSpeakersThatForgetToAccept() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val speakers = ApprovedProposal.allApprovedSpeakers()

      val proposals: Set[(Speaker, Iterable[Proposal])] = speakers.map {
        speaker =>
          (speaker, Proposal.allThatForgetToAccept(speaker.uuid).values)
      }.filter(_._2.nonEmpty)

      Ok(views.html.CFPAdmin.allSpeakersThatForgetToAccept(proposals))
  }

  // All speakers with a speaker's badge (it does not include Quickies, BOF and 3rd, 4th speakers)
  def allSpeakersWithAcceptedTalksAndBadge() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val speakers = ApprovedProposal.allApprovedSpeakers()
      val proposals: List[(Speaker, Iterable[Proposal])] = speakers.toList.map {
        speaker =>
          val allProposalsForThisSpeaker = Proposal.allApprovedAndAcceptedProposalsByAuthor(speaker.uuid).values
          val onIfFirstOrSecondSpeaker = allProposalsForThisSpeaker.filter(p => p.mainSpeaker == speaker.uuid || p.secondarySpeaker == Some(speaker.uuid))
            .filter(p => ProposalConfiguration.doesProposalTypeGiveSpeakerFreeEntrance(p.talkType))
          (speaker, onIfFirstOrSecondSpeaker)
      }.filter(_._2.nonEmpty)

      Ok(views.html.CFPAdmin.allSpeakersWithAcceptedTalksAndBadge(proposals))
  }

  // All speakers with a speaker's badge
  def allSpeakersWithAcceptedTalks() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val speakers = ApprovedProposal.allApprovedSpeakers()
      val proposals: List[(Speaker, Iterable[Proposal])] = speakers.toList.map {
        speaker =>
          val allProposalsForThisSpeaker = Proposal.allApprovedAndAcceptedProposalsByAuthor(speaker.uuid).values
          val onIfFirstOrSecondSpeaker = allProposalsForThisSpeaker.filter(p => p.mainSpeaker == speaker.uuid || p.secondarySpeaker == Some(speaker.uuid))
          (speaker, onIfFirstOrSecondSpeaker)
      }.filter(_._2.nonEmpty)

      Ok(views.html.CFPAdmin.allSpeakersWithAcceptedTalksAndBadge(proposals))
  }

  def allSpeakersWithAcceptedTalksForExport() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val speakers = ApprovedProposal.allApprovedSpeakers()
      val proposals: List[(Speaker, Iterable[Proposal])] = speakers.toList.map {
        speaker =>
          val allProposalsForThisSpeaker = Proposal.allApprovedAndAcceptedProposalsByAuthor(speaker.uuid).values
          val onIfFirstOrSecondSpeaker = allProposalsForThisSpeaker.filter(p => p.mainSpeaker == speaker.uuid || p.secondarySpeaker == Some(speaker.uuid))
          (speaker, onIfFirstOrSecondSpeaker)
      }.filter(_._2.nonEmpty)

      Ok(views.html.CFPAdmin.allSpeakersWithAcceptedTalksForExport(proposals))
  }

  def allWebusers() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val allSpeakers = Webuser.allSpeakers.sortBy(_.cleanName)
      Ok(views.html.CFPAdmin.allWebusers(allSpeakers))
  }


  import play.api.data.Form
  import play.api.data.Forms._

  def allCFPWebusers() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.CFPAdmin.showCFPUsers(Webuser.allCFPAdminUsers()))
  }

  def updateTrackLeaders() = SecuredAction(IsMemberOf("admin")) {
    implicit req: SecuredRequest[play.api.mvc.AnyContent] =>

      req.request.body.asFormUrlEncoded.map {
        mapsByTrack =>
          TrackLeader.updateAllTracks(mapsByTrack)
          Redirect(routes.CFPAdmin.allCFPWebusers).flashing("success" -> Messages("trackleaders.updated"))
      }.getOrElse {
        Redirect(routes.CFPAdmin.allCFPWebusers).flashing("error" -> "No value received")
      }
  }

  val editSpeakerForm = Form(
    tuple(
      "uuid" -> text.verifying(nonEmpty, maxLength(50)),
      "firstName" -> text.verifying(nonEmpty, maxLength(30)),
      "lastName" -> text.verifying(nonEmpty, maxLength(30))
    )
  )

  val speakerForm = Form(mapping(
    "uuid" -> optional(text),
    "email" -> (email verifying nonEmpty),
    "lastName" -> text,
    "bio2" -> nonEmptyText(maxLength = 1200),
    "lang2" -> optional(text),
    "avatarUrl" -> optional(text),
    "company2" -> optional(text),
    "blog2" -> optional(text),
    "firstName" -> text,
    "acceptTermsConditions" -> boolean,
    "qualifications2" -> nonEmptyText(maxLength = 750),
    "phone" -> optional(text),
    "gender2" -> optional(text),
    "tshirtSize2" -> optional(text),
    "tagName" -> nonEmptyText(maxLength = 50),
    "race2" -> optional(text),
    "disability2" -> optional(text),
    "socialMedia" -> mapping(
        "twitter2" -> optional(text.verifying(twitterURL)),
        "linkedIn2" -> optional(text.verifying(linkedInURL)),
        "github2" -> optional(text.verifying(githubURL)),
        "facebook2" -> optional(text.verifying(facebookURL)),
        "instagram2" -> optional(text.verifying(instagramURL))
      )(SocialMedia.apply)(SocialMedia.unapply)
  )(Speaker.createOrEditSpeaker)(Speaker.unapplyFormEdit))

  def newOrEditSpeaker(speakerUUID: Option[String]) = SecuredAction(IsMemberOfGroups(List("admin","cfp"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      speakerUUID match {
        case Some(uuid) => {
          Speaker.findByUUID(uuid).map {
            speaker: Speaker =>
			  if(Webuser.hasAccessToAdmin(request.webuser.uuid)) {
                Ok(views.html.CFPAdmin.newSpeaker(speakerForm.fill(speaker))).flashing("success" -> "You are currently editing an existing speaker")
			  } else {
			    Redirect(routes.Application.index()).flashing("error" -> "Not Authorized")
			  }
          }.getOrElse {
            Ok(views.html.CFPAdmin.newSpeaker(speakerForm)).flashing("error" -> "Speaker not found")
          }
        }
        case None => Ok(views.html.CFPAdmin.newSpeaker(speakerForm))
      }
  }

  def saveNewSpeaker() = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      speakerForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.CFPAdmin.newSpeaker(invalidForm)).flashing("error" -> "Invalid form, please check and correct errors. "),
        validSpeaker => {
          Option(validSpeaker.uuid) match {
            case Some(existingUUID) => {
              play.Logger.of("application.CFPAdmin").debug("Updating existing speaker " + existingUUID)
              Webuser.findByUUID(existingUUID).map {
                existingWebuser =>
                  if(existingWebuser.email != validSpeaker.email) {
                    Webuser.updateEmail(existingWebuser, validSpeaker.email)
                  } else {
                    Webuser.updateNames(existingUUID, validSpeaker.firstName.getOrElse("?"), validSpeaker.name.getOrElse("?"))
                  }
              }.getOrElse {
                val newWebuser = Webuser.createSpeaker(validSpeaker.email, validSpeaker.firstName.getOrElse("?"), validSpeaker.name.getOrElse("?"))
                val newUUID = Webuser.saveAndValidateWebuser(newWebuser)
                play.Logger.warn("Created missing webuser " + newUUID)
              }
              Speaker.save(validSpeaker)
              Event.storeEvent(Event(validSpeaker.cleanName, request.webuser.uuid, "updated a speaker [" + validSpeaker.uuid + "]"))
              TDCClient.updateSpeakerProfile(validSpeaker,Option(request.webuser.uuid))
              Redirect(routes.CFPAdmin.showSpeakerAndTalks(existingUUID)).flashing("success" -> "Profile updated")
            }
            case None => {
              val webuser = Webuser.createSpeaker(validSpeaker.email, validSpeaker.firstName.getOrElse("Firstname"), validSpeaker.name.getOrElse("Lastname"))
              Webuser.saveNewWebuserEmailNotValidated(webuser)
              val newUUID = Webuser.saveAndValidateWebuser(webuser)
              Speaker.save(validSpeaker.copy(uuid = newUUID))
              Event.storeEvent(Event(validSpeaker.cleanName, request.webuser.uuid, "created a speaker [" + validSpeaker.uuid + "]"))
              Redirect(routes.CFPAdmin.showSpeakerAndTalks(newUUID)).flashing("success" -> "Profile saved")
            }
          }
        }
      )
  }

  def setPreferredDay(proposalId: String, day: String) = SecuredAction(IsMemberOf("cfp")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Proposal.setPreferredDay(proposalId: String, day: String)
      Redirect(routes.CFPAdmin.openForReview(proposalId)).flashing("success" -> ("Preferred day set to " + day))
  }

  def resetPreferredDay(proposalId: String) = SecuredAction(IsMemberOf("cfp")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Proposal.resetPreferredDay(proposalId: String)
      Redirect(routes.CFPAdmin.openForReview(proposalId)).flashing("success" -> "No preferences")
  }

  def showProposalsWithNoVotes() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val proposals = Review.allProposalsWithNoVotes
      Ok(views.html.CFPAdmin.showProposalsWithNoVotes(proposals))
  }

  def history(proposalId:String)=SecuredAction(IsMemberOfGroups(List("cfp","admin"))){
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Proposal.findById(proposalId).map{
        proposal:Proposal=>
          Ok(views.html.CFPAdmin.history(proposal))
      }.getOrElse(NotFound("Proposal not found"))
  }
  /*
   * Loads the Speaker Events from the database and shows the speaker history view
   */
  def speakerHistory(uuidSpeaker: String)=SecuredAction(IsMemberOfGroups(List("cfp","admin"))){
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Speaker.findByUUID(uuidSpeaker).map{ speaker:Speaker =>
          val events = Event.loadEventsForObjRef(uuidSpeaker).sortBy(_.date.map(_.getMillis).getOrElse(0L))
          Ok(views.html.CFPAdmin.speakerHistory(speaker.cleanName,events))
      }.getOrElse(NotFound("Speaker not found"))
  }


  def help() = SecuredAction(IsMemberOf("cfp")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.CFPAdmin.help())
  }

  /*
   * loads the events and show the events view
   */
  def eventLog(page:Int) = SecuredAction(IsMemberOf("admin")){
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val twentyEvents = Event.loadEvents(20, page)
      Ok(views.html.Backoffice.showEvents(twentyEvents, Event.totalEvents(), page))
  }

  /**
    * loads all the talks that were marked as candidates for stadium, classified by track
    * @return
    */
  def allStadiumTalks() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>

      val allProposals = Proposal.allStadium()

      val groupedProposals:Map[Track,List[Proposal]] = allProposals.groupBy(_.track)

      Ok(views.html.CFPAdmin.allStadiumTalks(groupedProposals))
  }

  /**
    * Opens the form for the insertion of a new incident for a speaker
    */
  def addIncident(speakerId: String) = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.CFPAdmin.incident(incidentForm.fill(Incident(speakerId,IncidentType.UNKNOWN,conference=ConferenceDescriptor.current().eventCode))))
  }

  /**
    * Saves a new incident for the speaker
    *
    */
  def saveIncident() = SecuredAction(IsMemberOfGroups(List("cfp","admin"))) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      incidentForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.CFPAdmin.incident(invalidForm)).flashing("error" -> Messages("form.msg.error")),
        validIncident => {
          Incident.save(validIncident)
          Event.storeEvent(Event(validIncident.speakerId, request.webuser.uuid, "added a new incident [" + validIncident.incidentType.id + "]"))
          Redirect(routes.CFPAdmin.showSpeakerAndTalks(validIncident.speakerId)).flashing("success" -> Messages("sw.incidents.msg.success"))
        }
      )
  }


  /**
    * Deletes a vote for a proposal
    *
    * @param proposalId proposal that will have a vote deleted
    * @param reviewerUUID webuser that will have his vote deleted for the selected proposal
    * @return
    */
  def deleteVotesForProposal(proposalId:String,reviewerUUID:String) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(_) => {
          Review.removeVoteForProposal(uuid, proposalId, reviewerUUID)
          NoContent
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }
  
  /**
    * Shows report of speakers by gender
    *
    */
  def allSpeakersByGender() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val allSpeakers = Speaker.allSpeakers
      
      val allApprovedIDs= ApprovedProposal.allApprovedSpeakerIDs()
      val allRejectedIDs= ApprovedProposal.allRefusedSpeakerIDs()
      val refusedSpeakers = allRejectedIDs.diff(allApprovedIDs)
      val approvedSpeakers = allSpeakers.filter(s => allApprovedIDs.contains(s.uuid))
      val rejectedSpeakers = allSpeakers.filter(s => refusedSpeakers.contains(s.uuid))
      
      val speakersByTrackAndGender = Proposal.allAccepted()             //all accepted proposals
                              .groupBy(_.track)                         //proposals by track
                              .mapValues( _.flatMap( _.allSpeakers))    //speakers by track 		
                              .mapValues( _.groupBy(s => 
                                        s.gender.getOrElse("empty"))
                                        .withDefaultValue(List()))      //speakers by gender by track

      Ok(views.html.CFPAdmin.allSpeakersByGender(allSpeakers,approvedSpeakers,rejectedSpeakers,speakersByTrackAndGender))	
  }
  /**
  * Shows report of speakers by language
  */
  def allSpeakersByLang(lang:Option[String]) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val proposals = Proposal.allActiveProposals()
      val allSpeakers = proposals.flatMap(_.allSpeakers).toSet
      val speakersByLang = allSpeakers.toList
                                      .sortBy(_.cleanName)
                                      .groupBy(_.cleanLang)
      Ok(views.html.CFPAdmin.allSpeakersByLang(speakersByLang,lang.getOrElse("")))
  }
  
  /**
  * Shows report of uploaded presentations
  */
  def allUploadedPresentationsByTrack() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val eventCode = ConferenceDescriptor.current().eventCode
      val urls = S3.getUploadedPresentations(eventCode)
      val pattern = raw".+/presentations/${eventCode}/(\w+)/(\w{3}-\d{4})_.+".r
      
      val links:Map[String,Map[Option[Proposal],List[String]]] = urls.map(url => url match {
        case pattern(trackId,talk) => (trackId,talk,url)
      }).groupBy(_._1) //groups by track
        .mapValues(_.map(tuple => (tuple._2,tuple._3))) //strips the track from the values list
        .mapValues(_.groupBy(_._1).mapValues(_.map(_._2))) //groups by talk and strips it from the resulting list
        .mapValues(_.map{ case(talk,listOfLinks) =>
          (Proposal.findById(talk),listOfLinks)
        }) //replaced the talk id with the proposal object
        Ok(views.html.CFPAdmin.allUploadedPresentations(links))
  }
  
  /**
  * Shows report of all talks by area
  */
  def allTalksByArea = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val allProposalIds = Proposal.allProposalIDsNotDeleted
      val groupedProposals:Map[Track,List[Proposal]] = Proposal.loadAndParseProposals(allProposalIds)
                                                               .values
                                                               .toList
                                                               .groupBy(_.track)

      val allTrackAreas:Map[TrackArea,Map[Track,List[Proposal]]] = 
                          TrackArea.allAreas()
                                   .map(trackArea => (trackArea,groupedProposals.filterKeys(trackArea.tracks.contains(_))))
                                   .toMap
  

      Ok(views.html.CFPAdmin.allTalksByArea(allTrackAreas))      
  }

  /**
    * Show report with all talks that have been approved but can't be recorded. This report is important
    * to select the talks for the Stadium track, since this track is recorded.
    */
  def allApprovedTalksRecordingNotAllowed() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val allApprovedTalks = ApprovedProposal.allApproved()
                                             .filter(talk => !talk.publicationAuthorized.getOrElse(false))
      Ok(views.html.CFPAdmin.allTalksRecordingNotAllowed(allApprovedTalks.toList))
  }
}



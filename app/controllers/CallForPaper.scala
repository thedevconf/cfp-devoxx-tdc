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

import library.search.ElasticSearch
import library.{NotifyProposalSubmitted, ProfileUpdated, SendMessageToCommitte, ZapActor, UploadPresentation, S3, ApprovedProposalUpdated}
import models._
import models.SocialMedia._
import org.apache.commons.lang3.StringUtils
import play.api.cache.Cache
import play.api.data.Forms._
import play.api.data._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.libs.Crypto
import play.api.libs.json.Json

/**
  * Main controller for the speakers.
  *
  * Author: nicolas martignole
  * Created: 29/09/2013 12:24
  */
object CallForPaper extends SecureCFPController {

  def homeForSpeaker = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      Ok(views.html.Authentication.confirmImport(speakerForm))
      Speaker.findByUUID(uuid).map {
        speaker: Speaker =>
          val hasApproved = Proposal.countByProposalState(uuid, ProposalState.APPROVED) > 0
          val hasAccepted = Proposal.countByProposalState(uuid, ProposalState.ACCEPTED) > 0
          val needsToAcceptTermAndCondition = Speaker.needsToAccept(uuid) && (hasAccepted || hasApproved)
          val hasUnconfirmedBackup = Proposal.existsUnconfirmedBackupForSpeaker(uuid)

          (needsToAcceptTermAndCondition, hasApproved || hasUnconfirmedBackup, hasAccepted) match {
            case (true, _, _) => Redirect(routes.ApproveOrRefuse.acceptTermsAndConditions())
            case (false, true, _) => Redirect(routes.ApproveOrRefuse.doAcceptOrRefuseTalk()).flashing("success" -> Messages("please.check.approved"))
            case other => {
              val allProposals = Proposal.allMyProposals(uuid)
              val archivedProposals = Proposal.allMyArchivedProposals(uuid)
              val ratings = if(hasAccepted||hasApproved){
                Rating.allRatingsForTalks(allProposals)
              }else{
                Map.empty[Proposal,List[Rating]]
              }
              Ok(views.html.CallForPaper.homeForSpeaker(speaker, request.webuser, allProposals, archivedProposals,ratings))
            }
          }
      }.getOrElse {
        val flashMessage = if (Webuser.hasAccessToGoldenTicket(request.webuser.uuid)) {
          Messages("callforpaper.gt.create.profile")
        } else {
          Messages("callforpaper.import.profile")
        }
        //We have a Webuser but no associated Speaker profile v
        Redirect(routes.CallForPaper.newSpeakerForExistingWebuser()).flashing("success" -> flashMessage)
      }
  }

  // Specific secured action. We need a redirect from homeForSpeaker, to be able to display flash message
  def newSpeakerForExistingWebuser = SecuredAction {
    implicit request =>
      val w = request.webuser
      val defaultValues = 
            Speaker(uuid = "xxx"
               , email = w.email 
               , name = Option(w.lastName)
               , bio = StringUtils.abbreviate("...", 750)
               , lang = None
               , twitter = None
               , avatarUrl = None
               , company = None
               , blog = None
               , firstName = Option(w.firstName)
               , qualifications = Option("No experience")
               , phone = None
               , location =
               , gender = None
               , tshirtSize = None
               , linkedIn = None
               , github = None
               , tagName = None
               , facebook = None
               , instagram = None
               , race = None
               , disability = None
            )
      Ok(views.html.Authentication.confirmImport(Authentication.importSpeakerForm.fill(defaultValues)))
  }

  val speakerForm = Form(mapping(
    "uuid" -> ignored("xxx"),
    "email" -> (email verifying nonEmpty),
    "lastName" -> nonEmptyText(maxLength = 25),
    "bio" -> nonEmptyText(maxLength = 750),
    "lang" -> optional(text),
    "avatarUrl" -> optional(text),
    "company" -> optional(text),
    "blog" -> optional(text),
    "firstName" -> nonEmptyText(maxLength = 25),
    "qualifications" -> nonEmptyText(maxLength = 750),
    "phone" -> nonEmptyText,
    "gender" -> optional(text),
    "tshirtSize" -> optional(text),
    "tagName" -> nonEmptyText(maxLength = 50),
    "race" -> optional(text),
    "disability" -> optional(text),
    "socialMedia" -> mapping(
      "twitter" -> optional(text.verifying(twitterURL)),
      "linkedIn" -> optional(text.verifying(linkedInURL)),
      "github" -> optional(text.verifying(githubURL)),
      "facebook" -> optional(text.verifying(facebookURL)),
      "instagram" -> optional(text.verifying(instagramURL))
    )(SocialMedia.apply)(SocialMedia.unapply)
  )(Speaker.createSpeaker)(Speaker.unapplyForm))

  def editProfile = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      Speaker.findByUUID(uuid).map {
        speaker =>
          Ok(views.html.CallForPaper.editProfile(speakerForm.fill(speaker), uuid))
      }.getOrElse(Unauthorized("User not found"))
  }

  def saveProfile = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      speakerForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.CallForPaper.editProfile(invalidForm, uuid)).flashing("error" -> "Invalid form, please check and correct errors. "),
        updatedSpeaker => {
          Webuser.updateNames(uuid, updatedSpeaker.firstName.getOrElse("?"), updatedSpeaker.name.getOrElse("?"))
          Speaker.update(uuid, updatedSpeaker)
          ZapActor.actor ! ProfileUpdated(updatedSpeaker.copy(uuid=uuid))
          Redirect(routes.CallForPaper.homeForSpeaker()).flashing("success" -> "Profile saved")
        }
      )
  }

  // Load a new proposal form
  def newProposal() = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      Ok(views.html.CallForPaper.newProposal(Proposal.proposalForm)).withSession(request.session + ("token" -> Crypto.sign(uuid)))
  }

  // Load a proposal
  def editProposal(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findProposal(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          if (proposal.mainSpeaker == uuid) {
            val proposalForm = Proposal.proposalForm.fill(proposal)
            Ok(views.html.CallForPaper.newProposal(proposalForm)).withSession(request.session + ("token" -> Crypto.sign(proposalId)))
          } else if (proposal.secondarySpeaker.isDefined && proposal.secondarySpeaker.get == uuid) {
            // Switch the mainSpeaker and the other Speakers
            val proposalForm = Proposal.proposalForm.fill(Proposal.setMainSpeaker(proposal, uuid))
            Ok(views.html.CallForPaper.newProposal(proposalForm)).withSession(request.session + ("token" -> Crypto.sign(proposalId)))
          } else if (proposal.otherSpeakers.contains(uuid)) {
            // Switch the secondary speaker and this speaker
            val proposalForm = Proposal.proposalForm.fill(Proposal.setMainSpeaker(proposal, uuid))
            Ok(views.html.CallForPaper.newProposal(proposalForm)).withSession(request.session + ("token" -> Crypto.sign(proposalId)))
          } else {
            Redirect(routes.CallForPaper.homeForSpeaker()).flashing("error" -> "Invalid state")
          }
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  // Prerender the proposal, but do not persist
  def previewProposal() = SecuredAction {
    implicit request =>
      Proposal.proposalForm.bindFromRequest.fold(
        hasErrors => BadRequest(views.html.CallForPaper.newProposal(hasErrors)).flashing("error" -> "invalid.form"),
        validProposal => {
          val summary = validProposal.summaryAsHtml // markdown to HTML
          val privateMessage = validProposal.privateMessageAsHtml // markdown to HTML
          Ok(views.html.CallForPaper.previewProposal(summary, privateMessage, Proposal.proposalForm.fill(validProposal), request.webuser.uuid))
        }
      )
  }

  // Revalidate to avoid CrossSite forgery and save the proposal
  def saveProposal() = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid

      Proposal.proposalForm.bindFromRequest.fold(
        hasErrors => BadRequest(views.html.CallForPaper.newProposal(hasErrors)),
        proposal => {
          // If the editor is not the owner then findProposal returns None
          Proposal.findProposal(uuid, proposal.id) match {
            case Some(existingProposal) => {
              // This is an edit operation
              // First we try to reset the speaker's, we do not take the values from the FORM for security reason
              val updatedProposal = proposal.copy(mainSpeaker = existingProposal.mainSpeaker, secondarySpeaker = existingProposal.secondarySpeaker, otherSpeakers = existingProposal.otherSpeakers)

              // Then because the editor becomes mainSpeaker, we have to update the secondary and otherSpeaker
              Proposal.save(uuid, Proposal.setMainSpeaker(updatedProposal, uuid), existingProposal.state)
              Event.storeEvent(Event(proposal.id, uuid, "Updated proposal " + proposal.id + " with current state [" + existingProposal.state.code + "]"))
              
              // Notifies the TDC application if the state of the proposal is approved or accepted to keep the TDC site in sync with the cfp
              if(existingProposal.state == ProposalState.APPROVED || existingProposal.state == ProposalState.ACCEPTED) {
                ZapActor.actor ! ApprovedProposalUpdated(updatedProposal)
              }
              Redirect(routes.CallForPaper.homeForSpeaker()).flashing("success" -> Messages("saved2"))
            }
            case other => {
              // Check that this is really a new id and that it does not exist
              if (Proposal.isNew(proposal.id)) {
                // This is a "create new" operation
                Proposal.save(uuid, proposal, ProposalState.DRAFT)
                Event.storeEvent(Event(proposal.id, uuid, "Created a new proposal " + proposal.id + " with title " + StringUtils.abbreviate(proposal.title, 80)))
                Redirect(routes.CallForPaper.homeForSpeaker).flashing("success" -> Messages("saved"))
              } else {
                // Maybe someone tried to edit someone's else proposal...
                Event.storeEvent(Event(proposal.id, uuid, "Tried to edit this talk but he is not the owner."))
                Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> "You are trying to edit a proposal that is not yours. This event has been logged.")
              }
            }
          }
        }
      )
  }

  // Load a proposal by its id
  def editOtherSpeakers(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findProposal(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          if (proposal.mainSpeaker == uuid) {
            val proposalForm = Proposal.proposalSpeakerForm.fill(proposal.secondarySpeaker, proposal.otherSpeakers)
            Ok(views.html.CallForPaper.editOtherSpeaker(Webuser.getName(uuid), proposal, proposalForm))
          } else if (proposal.secondarySpeaker.isDefined && proposal.secondarySpeaker.get == uuid) {
            // Switch the mainSpeaker and the other Speakers
            val proposalForm = Proposal.proposalSpeakerForm.fill(Option(proposal.mainSpeaker), proposal.otherSpeakers)
            Ok(views.html.CallForPaper.editOtherSpeaker(Webuser.getName(uuid), proposal, proposalForm))
          } else if (proposal.otherSpeakers.contains(uuid)) {
            // let this speaker as a member of the third list
            Redirect(routes.CallForPaper.homeForSpeaker()).flashing("error" -> Messages("speaker.other.error"))
          } else {
            Redirect(routes.CallForPaper.homeForSpeaker()).flashing("error" -> "Invalid state")
          }
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  // Check that the current authenticated user is the owner
  // validate the form, save and redirect.
  def saveOtherSpeakers(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findProposal(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          Proposal.proposalSpeakerForm.bindFromRequest.fold(
            hasErrors => BadRequest(views.html.CallForPaper.editOtherSpeaker(Webuser.getName(uuid), proposal, hasErrors)).flashing("error" -> "Errors in the proposal form, please correct errors"),
            validNewSpeakers => {
              (proposal.secondarySpeaker, validNewSpeakers._1) match {
                case (None, Some(newSecondarySpeaker)) =>
                  val newSpeaker = Speaker.findByUUID(newSecondarySpeaker)
                  val validMsg = s"Internal notification : Added [${newSpeaker.map(_.cleanName).get}] as a secondary speaker"
                  if (proposal.state != ProposalState.DRAFT) {
                    ZapActor.actor ! SendMessageToCommitte(uuid, proposal, validMsg)
                  }
                  Event.storeEvent(Event(proposal.id, uuid, validMsg))
                  Proposal.updateSecondarySpeaker(uuid, proposalId, None, Some(newSecondarySpeaker))
                case (Some(oldSpeakerUUID), Some(newSecondarySpeaker)) if oldSpeakerUUID != newSecondarySpeaker =>
                  val oldSpeaker = Speaker.findByUUID(oldSpeakerUUID)
                  val newSpeaker = Speaker.findByUUID(newSecondarySpeaker)
                  val validMsg = s"Internal notification : Removed [${oldSpeaker.map(_.cleanName).get}] and added [${newSpeaker.map(_.cleanName).get}] as a secondary speaker"
                  if (proposal.state != ProposalState.DRAFT) {
                    ZapActor.actor ! SendMessageToCommitte(uuid, proposal, validMsg)
                  }
                  Event.storeEvent(Event(proposal.id, uuid, validMsg))
                  Proposal.updateSecondarySpeaker(uuid, proposalId, Some(oldSpeakerUUID), Some(newSecondarySpeaker))
                case (Some(oldSpeakerUUID), None) =>
                  val oldSpeaker = Speaker.findByUUID(oldSpeakerUUID)
                  val validMsg = s"Internal notification : Removed [${oldSpeaker.map(_.cleanName).get}] as a secondary speaker"
                  if (proposal.state != ProposalState.DRAFT) {
                    ZapActor.actor ! SendMessageToCommitte(uuid, proposal, validMsg)
                  }
                  Event.storeEvent(Event(proposal.id, uuid, validMsg))
                  Proposal.updateSecondarySpeaker(uuid, proposalId, Some(oldSpeakerUUID), None)
                case (Some(oldSpeakerUUID), Some(newSecondarySpeaker)) if oldSpeakerUUID == newSecondarySpeaker =>
                // We kept the 2nd speaker, maybe updated or added a 3rd speaker
                case (None, None) =>
                // Nothing special
              }

              Proposal.updateOtherSpeakers(uuid, proposalId, proposal.otherSpeakers, validNewSpeakers._2)
              Event.storeEvent(Event(proposal.id, uuid, "Updated speakers list for proposal " + StringUtils.abbreviate(proposal.title, 80)))

              Redirect(routes.CallForPaper.homeForSpeaker).flashing("success" -> Messages("speakers.updated"))
            }
          )
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  def deleteProposal(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findProposal(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          Proposal.delete(uuid, proposalId)
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("deleted" -> proposalId)
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  def undeleteProposal(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findDeleted(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          Proposal.draft(uuid, proposalId)
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("success" -> Messages("talk.draft"))
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  def submitProposal(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findDraft(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          Proposal.submit(uuid, proposalId)
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("success" -> Messages("talk.submitted"))
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  val speakerMsg = Form("msg" -> nonEmptyText(maxLength = 2500))

  def showCommentForProposal(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findProposal(uuid, proposalId)
      maybeProposal match {
        case Some(proposal) => {
          Ok(views.html.CallForPaper.showCommentForProposal(proposal, Comment.allSpeakerComments(proposal.id), speakerMsg))
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  def sendMessageToCommitte(proposalId: String) = SecuredAction {
    implicit request =>
      val uuid = request.webuser.uuid
      val maybeProposal = Proposal.findProposal(uuid, proposalId).filterNot(_.state == ProposalState.DELETED)
      maybeProposal match {
        case Some(proposal) => {
          speakerMsg.bindFromRequest.fold(
            hasErrors => {
              BadRequest(views.html.CallForPaper.showCommentForProposal(proposal, Comment.allSpeakerComments(proposal.id), hasErrors))
            },
            validMsg => {
              Comment.saveCommentForSpeaker(proposal.id, uuid, validMsg)
              ZapActor.actor ! SendMessageToCommitte(uuid, proposal, validMsg)
              Redirect(routes.CallForPaper.showCommentForProposal(proposalId)).flashing("success" -> "Message was sent")
            }
          )
        }
        case None => {
          Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("invalid.proposal"))
        }
      }
  }

  case class TermCount(term: String, count: Int)

  def cloudTags() = SecuredAction.async {
    implicit request =>
      import play.api.Play.current
      import play.api.libs.concurrent.Execution.Implicits.defaultContext

      implicit val termCountFormat = Json.reads[TermCount]

      Cache.getOrElse("elasticSearch", 3600) {
        ElasticSearch.getTag("proposals/proposal").map {
          case r if r.isSuccess => {
            val json = Json.parse(r.get)
            val tags = (json \ "facets" \ "tags" \ "terms").as[List[TermCount]]
            Ok(views.html.CallForPaper.cloudTags(tags))
          }
          case r if r.isFailure => {
            play.Logger.error(r.get)
            InternalServerError
          }
        }
      }
  }

  /**
    * Opens the form used for the upload of the proposal presentation
    * @param proposalId
    * @return
    */
  def uploadPresentationForProposal(proposalId:String) = SecuredAction {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.CallForPaper.uploadPresentation(proposalId))
  }

  /**
    * Reads the presentation for the proposal from the web application and saves it to a temporary file
    * defined by S3.presentationSourceDir, prefixed with the current date and the proposal id
    *
    * Also updates the status of the proposal to inform that it already has an uploaded presentation
    *
    * @param proposalId
    * @return
    */
  def savePresentationForProposal(proposalId:String) = SecuredAction {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      request.body.asMultipartFormData.map { data =>

        import java.time.LocalDateTime
        import java.time.format.DateTimeFormatter
        import java.io.File

        data.file("presentation").map{ presentation =>
          val prefix = proposalId + "_" + LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'hhmmss"))
          val filename = s"${prefix}_${presentation.filename}"
          presentation.ref.moveTo(new File(S3.presentationSourceDir + filename))
          Proposal.updatePresentationStatus(proposalId,true)
          ZapActor.actor ! UploadPresentation(proposalId, filename, request.webuser.uuid)
          Redirect(routes.CallForPaper.homeForSpeaker()).flashing("success" -> Messages("uploadPresentation.msg.success"))
        }.getOrElse(
          Redirect(routes.CallForPaper.homeForSpeaker()).flashing("error" -> Messages("uploadPresentation.msg.error.missing"))
        )
      }.getOrElse {
        Redirect(routes.CallForPaper.homeForSpeaker()).flashing("error" -> Messages("uploadPresentation.msg.error.general"))
      }
  }
  
  /**
    * Reads the picture for speaker and saves it to a temporary file defined by S3.pictureSourceDir
    *
    * @param userId
    * @return JSON response for the fineUploader component
    */
  def uploadPicture(userId:String) = SecuredAction {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      request.body.asMultipartFormData.map { data =>

        import java.io.File

        data.file("qqfile").map{ picture =>
          picture.ref.moveTo(new File(S3.pictureSourceDir + picture.filename),replace = true)
          val avatarUrl = S3.uploadPicture(userId,picture.filename)
          Event.storeEvent(Event(userId, request.webuser.uuid, "Photo uploaded"))		  
          Ok(s"""{
            "success":true,
            "avatarUrl": "${avatarUrl}"
            }""")
        }.getOrElse{
          BadRequest(s"""{"success":false,"error":"${Messages("uploadPicture.msg.error.missing")}"}""")
        }
      }.getOrElse {
        BadRequest(s"""{"success":false,"error":"${Messages("uploadPicture.msg.error.general")}"}""")
      }
  }

  /**
  * Show the contents of an archived Proposal of the speaker
  *
  **/
  def showArchivedProposal(proposalId: String) = SecuredAction {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val uuid = request.webuser.uuid
      Proposal.findById(proposalId) match {
        case Some(proposal) => {
            val authorIds: List[String] = proposal.mainSpeaker :: proposal.secondarySpeaker.toList ::: proposal.otherSpeakers
            //checks whether the user is one of the authors of the proposal
            if(authorIds.contains(uuid)) {
                Ok(views.html.CallForPaper.showArchivedProposal(proposal))
            } else {
                Redirect(routes.CallForPaper.homeForSpeaker).flashing("error" -> Messages("msg.unauthorized"))
            }
        }
        case None => NotFound("Proposal not found").as("text/html")
      }
  }
}


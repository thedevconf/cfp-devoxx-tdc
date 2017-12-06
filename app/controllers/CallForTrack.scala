package controllers

import controllers.CallForPaper.{Ok, session}
import models.{Proposal, TrackProposal, Webuser, WorkshopProposal}
import play.api.libs.Crypto

object CallForTrack extends SecureCFPController{

  def homeCallForTracks() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.CallForTrack.homeCallForTrack())
  }

  def newTrackProposal() = SecuredAction(IsMemberOf("admin")) {
    implicit request =>
      val uuid = request.webuser.uuid
      Ok(views.html.CallForTrack.newTrack(TrackProposal.proposalForm,Webuser.allCFPAdminUsers())).withSession(session + ("token" -> Crypto.sign(uuid)))
  }

  def newWorkshopProposal() = SecuredAction(IsMemberOf("admin")) {
    implicit request =>
      val uuid = request.webuser.uuid
      Ok(views.html.CallForTrack.newWorkshop(WorkshopProposal.proposalForm, Webuser.allCFPAdminUsers())).withSession(session + ("token" -> Crypto.sign(uuid)))
  }

  def saveTrack() = TODO

  def saveWorkshop() = TODO
}

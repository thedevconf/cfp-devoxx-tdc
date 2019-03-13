package controllers

import models.{ConferenceDescriptor,Track}
import models.Track._

/**
  * The conference controller for the CFP technical committee.
  *
  * Author: @kxavier
  */
object ConferenceController extends SecureCFPController {

    /**
     * lists all the tracks for the conference
     */
    def allTracks() = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        val allTracks = Track.allTracks.sortBy(_.id)
        Ok(views.html.Backoffice.showConferenceTracks(allTracks))
    }

    /**
     * opens the track form
     */
    def newOrEditTrack(trackId: Option[String]) = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        trackId match {
          case Some(id) => 
            val optionTrack = Track.load(id)
            optionTrack.map{ track =>
              val form = trackForm.fill(track)
              Ok(views.html.Backoffice.editTrack(form))
            }.getOrElse(NotFound("Track not found").as("text/html"))	
          case None => Ok(views.html.Backoffice.editTrack(trackForm))
        }
    }
    /**
     * saves a track in the database
     */
    def saveTrack() = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        trackForm.bindFromRequest.fold(
          invalidForm => BadRequest(views.html.Backoffice.editTrack(invalidForm)),
          track => {
            Track.save(track)
            Redirect(routes.ConferenceController.allTracks)
          }  
        )
    }
}
package controllers

import models.{ConferenceDescriptor,Track,TrackArea}
import models.Track._
import models.TrackArea._
import play.api.i18n.Messages

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
  /**
   * lists all the track areas
   */
  def allTrackAreas() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val allAreas = TrackArea.allAreas().toList.sortBy(_.description)
      Ok(views.html.Backoffice.showTrackAreas(allAreas))
  } 

  /**
   * Saves a track area
   */
  def updateTracksForAreas() = SecuredAction(IsMemberOf("admin")) {
    implicit req: SecuredRequest[play.api.mvc.AnyContent] =>
       req.request.body.asFormUrlEncoded.map {
        tracksByArea =>
          TrackArea.updateAllAreas(tracksByArea)
          Redirect(routes.ConferenceController.allTrackAreas).flashing("success" -> Messages("backoffice.area.msg.updated"))
      }.getOrElse {
        Redirect(routes.ConferenceController.allTrackAreas).flashing("error" -> "No value received")
      }
  } 

  /**
  * opens the track area form page
  */
  def newOrEditArea(areaId:Option[String]) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      areaId match {
        case Some(id) => 
          val optionArea = TrackArea.load(id)
          optionArea.map{ area =>
            val form = areaForm.fill(area)
            Ok(views.html.Backoffice.editTrackArea(form))
          }.getOrElse(NotFound("Area not found").as("text/html"))	
        case None => Ok(views.html.Backoffice.editTrackArea(areaForm))
      }
  }

  /**
  * saves an area
  */
  def saveArea() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      areaForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.Backoffice.editTrackArea(invalidForm)),
        trackArea => {
          TrackArea.save(trackArea)	
          Redirect(routes.ConferenceController.allTrackAreas)
        }  
      )
  }  


}
package controllers

import models.{ConferenceDescriptor,Track,TrackArea,TDCConference}
import models.Track._
import models.TrackArea._
import models.TDCConference._
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
     * deletes a track from the database
     */
    def deleteTrack(trackPrimaryKey: String) = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        Track.delete(trackPrimaryKey)
        Redirect(routes.ConferenceController.allTracks)
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

    /**
     * lists all the conferences
     */
    def allConferences() = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        val allConferences = TDCConference.allConferences.sortBy(_.eventCode)
        Ok(views.html.Backoffice.showAllConferences(allConferences))
    }

    /**
     * opens the conference form
     */
    def newOrEditConference(conferenceId: Option[String]) = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        conferenceId match {
          case Some(eventCode) => 
            val optionConference = TDCConference.load(eventCode)
            optionConference.map{ conference =>
              val form = conferenceForm.fill(conference)
              Ok(views.html.Backoffice.editConference(form))
            }.getOrElse(NotFound(Messages("backoffice.conferences.error.noconference",conferenceId))).as("text/html")
          case None => Ok(views.html.Backoffice.editConference(conferenceForm))
        }
    }

    /**
     * saves a conference in the database
     */
    def saveConference() = SecuredAction(IsMemberOf("admin")) {
      implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
        conferenceForm.bindFromRequest.fold(
          invalidForm => BadRequest(views.html.Backoffice.editConference(invalidForm)),
          conference => {
            TDCConference.save(conference)

            //need to reset the selected descriptor in the cache
            ConferenceDescriptor.clearCache(conference.eventCode)

            Redirect(routes.ConferenceController.allConferences)
          }  
        )
    }

  /**
   * opens or closes the call for papers for the conference
   */
  def openCallForPapers(conferenceId:String, open:Boolean) = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      val optConference = TDCConference.load(conferenceId)
      optConference.map( oldConference => {
        TDCConference.save(oldConference.copy(cfpOpen = Option(open)))

        //need to reset the selected descriptor in the cache
        ConferenceDescriptor.clearCache(conferenceId)

        val allConferences = TDCConference.allConferences.sortBy(_.eventCode)
        Ok(views.html.Backoffice.showAllConferences(allConferences))
      }).getOrElse(
        Ok(views.html.Backoffice.showAllConferences(TDCConference.allConferences.sortBy(_.eventCode)))
          .flashing("error" -> Messages("backoffice.conferences.error.noconference",conferenceId))
      )
  }
  
  /**
   * selects the active conference
   */
  def selectConference(conferenceId:String) = SecuredAction {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
     ConferenceDescriptor.selectConference(conferenceId)
     Redirect(routes.CallForPaper.homeForSpeaker()).withSession("eventCode" -> conferenceId)
  }

}
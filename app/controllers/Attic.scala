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

import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages

import scala.concurrent.Future

/**
 * Attic service to archive conference and talks.
 * @author created by N.Martignole, Innoteria, on 14/11/2014.
 */
object Attic extends SecureCFPController {

  val opTypeForm = Form("opType" -> text)
  val proposalTypeForm = Form("proposalType" -> text)

  def atticHome() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Ok(views.html.Attic.atticHome())
  }

  /**
   * Either destroy [draft] or [deleted] proposals, using a Future, as this code is slow and blocks
   * @return
   */
  def prune() = SecuredAction(IsMemberOf("admin")).async {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      import scala.concurrent.ExecutionContext.Implicits.global

      opTypeForm.bindFromRequest().fold(hasErrors => Future.successful(BadRequest(views.html.Attic.atticHome())),
        (opType: String) => opType match {
          case o if o == "deleted" =>
            val futureTotalDeleted = Future(ArchiveProposal.pruneAllDeleted())
            futureTotalDeleted.map { totalDeleted =>
              Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.prune.deleted",totalDeleted)))
            }
          case o if o == "draft" =>
            val futureTotalDraft = Future(ArchiveProposal.pruneAllDraft())
            futureTotalDraft.map {
              totalDraft =>
                Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.prune.draft",totalDraft)))
            }
          case other =>
            Future.successful(Redirect(routes.Attic.atticHome()))
        }
      )
  }

  def doArchive() = SecuredAction(IsMemberOf("admin")).async {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      import scala.concurrent.ExecutionContext.Implicits.global
      Future(ArchiveProposal.archiveAll()).map {
        totalArchived =>
          Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.archive.talk",totalArchived)))
        }
  }

  def deleteInvitedSpeakers() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Invitation.deleteAll()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.delete.invited")))
  }

  /**
   * Reset the list of notified speakers.
   */
  def resetNotified() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Event.resetSpeakersNotified()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.reset.notifications")))
  }

  /**
   * Flush the logs and the Events. The Events is an Audit log. See Event model for more details.
   */
  def resetEvents() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Event.resetEvents()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.delete.events")))
  }

  /**
    * Flush the trackleaders.
    */
  def resetTrackLeaders() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      TrackLeader.resetAll()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.reset.trackleaders")))
  }

  /**
    * Flush the track schedules.
    */
  def resetSchedules() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      TDCScheduleConfiguration.deleteAll()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.reset.schedules")))
  }

  /**
    * Reset the list of confirmations for backup proposals.
    */
  def resetBackupConfirmations() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      Event.resetBackupConfirmations()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.reset.backup.confirmations")))
  }

   /**
    * Reset the list of track areas
    */
  def resetTrackAreas() = SecuredAction(IsMemberOf("admin")) {
    implicit request: SecuredRequest[play.api.mvc.AnyContent] =>
      TrackArea.resetAll()
      Redirect(routes.Attic.atticHome()).flashing(("success", Messages("attic.msg.reset.areas")))
  }
  
}

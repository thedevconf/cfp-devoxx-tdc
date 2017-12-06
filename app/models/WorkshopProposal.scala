package models

import models.Proposal.generateId
import play.api.data.Form
import play.api.data.Forms._

case class WorkshopProposal(id: String,
                            workshopType: String,
                            theme: String,
                            description:String,
                            minimalAge:String,
                            notebookNeeded:Boolean,
                            infrastructure: String,
                            topics: String,
                            prerequisites:String,
                            target:String,
                            otherInformation:String)


object WorkshopProposal {

  val proposalForm = Form(mapping(
    "id" -> optional(text)
    , "workshopType" -> nonEmptyText
    , "theme" -> nonEmptyText
    , "description" -> nonEmptyText
    , "minimalAge" -> nonEmptyText
    , "notebookNeeded" -> boolean
    , "infrastructure" -> nonEmptyText
    , "topics" -> nonEmptyText
    , "prerequisites" -> nonEmptyText
    , "target" -> nonEmptyText
    , "otherInformation" -> nonEmptyText
  )(validateNewProposal)(unapplyProposalForm))

  def validateNewProposal(id: Option[String],
                          workshopType: String,
                          theme: String,
                          description:String,
                          minimalAge:String,
                          notebookNeeded:Boolean,
                          infrastructure: String,
                          topics: String,
                          prerequisites:String,
                          target:String,
                          otherInformation:String): WorkshopProposal = {
    WorkshopProposal(
      id.getOrElse(generateId()),
      workshopType,
      theme,
      description,
      minimalAge,
      notebookNeeded,
      infrastructure,
      topics,
      prerequisites,
      target,
      otherInformation
    )
  }

  def unapplyProposalForm(w: WorkshopProposal): Option[(Option[String], String, String, String, String, Boolean,String, String, String, String,String)] = {
    Option((Option(w.id), w.workshopType, w.theme, w.description, w.minimalAge, w.notebookNeeded, w.infrastructure, w.topics, w.prerequisites, w.target, w.otherInformation))
  }

  val allTypes = List(
    "t1" -> "callfortracks.comboworkshoptype.1",
    "t2" -> "callfortracks.comboworkshoptype.2"
  )

  val materialTypes = List(
    "flipchart" -> "Flipchart",
    "quadroBranco" -> "Quadro Branco",
    "projetor" -> "Projetor",
    "postIt" -> "Post-its",
    "caneta" -> "Caneta",
    "folhaA4" -> "Folha A4"
  )

}

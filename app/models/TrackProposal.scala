package models

import models.Proposal.generateId
import play.api.data.Form
import play.api.data.Forms._

case class TrackProposal(id:String, name:String, slogan:String, description:String, target:String
                         , numberPeople:String, numberProposals:String, sponsors:String,otherGroups:String, publications:String )

object TrackProposal {

  val proposalForm = Form(mapping(
    "id" -> optional(text)
    , "name" -> nonEmptyText(maxLength = 16)
    , "slogan" ->  nonEmptyText
    , "description" -> nonEmptyText
    , "target" -> nonEmptyText
    , "numberPeople" -> nonEmptyText
    , "numberProposals" -> nonEmptyText
    , "sponsors" -> nonEmptyText
    , "otherGroups" -> nonEmptyText
    , "publications" -> nonEmptyText
 /**   , "secondarySpeaker" -> optional(text)
    , "otherSpeakers" -> list(text)
    , "talkType" -> nonEmptyText
    , "summary" -> nonEmptyText(maxLength = 200 + ConferenceDescriptor.current().maxProposalSummaryCharacters) // Add 20% characters for Markdown extra characters.
    , "privateMessage" -> nonEmptyText(maxLength = 3500)
    , "sponsorTalk" -> boolean
    , "track" -> nonEmptyText
    , "demoLevel" -> optional(text)
    , "userGroup" -> optional(boolean)**/
  )(validateNewTrackProposal)(unapplyTrackProposalForm))

  def validateNewTrackProposal(id: Option[String],
                               name: String,
                               slogan: String,
                               description: String,
                               target:String,
                               numberPeople:String,
                               numberProposals:String,
                               sponsors:String,
                               otherGroups:String,
                               publications:String
 /**                         secondarySpeaker: Option[String],
                          otherSpeakers: List[String],
                          talkType: String,
                          audienceLevel: String,
                          summary: String,
                          privateMessage: String,
                          sponsorTalk: Boolean,
                          track: String,
                          demoLevel: Option[String],
                          userGroup: Option[Boolean]**/): TrackProposal = {
    TrackProposal(
      id.getOrElse(generateId()),
      name, slogan, description, target, numberPeople, numberProposals, sponsors,otherGroups, publications
    )
  }

  def unapplyTrackProposalForm(p: TrackProposal): Option[(Option[String], String, String, String, String, String, String, String, String, String)] = {
    Option((Option(p.id), p.name, p.slogan, p.description, p.target, p.numberPeople, p.numberProposals, p.sponsors, p.otherGroups, p.publications))
  }

  val numberPeopleOptions = List(
    "o1" -> "callfortracks.combopeople.1",
    "o2" -> "callfortracks.combopeople.2",
    "o3" -> "callfortracks.combopeople.3",
    "o4" -> "callfortracks.combopeople.4",
    "o5" -> "callfortracks.combopeople.5",
    "o6" -> "callfortracks.combopeople.6"
    )

  val numberProposalsOptions = List(
    "o1" -> "callfortracks.comboproposals.1",
    "o1" -> "callfortracks.comboproposals.2",
    "o1" -> "callfortracks.comboproposals.3",
    "o1" -> "callfortracks.comboproposals.4",
    "o1" -> "callfortracks.comboproposals.5"
  )

}

package models

import java.util.{Date, Locale}

import org.joda.time.{LocalDate, Period}
import play.api.Play
import play.api.i18n.{Lang, Messages}

/**
  * ConferenceDescriptor.
  * This might be the first file to look at, and to customize.
  * Idea behind this file is to try to collect all configurable parameters for a conference.
  *
  * For labels, please do customize messages and messages.fr
  *
  * Note from Nicolas : the first version of the CFP was much more "static" but hardly configurable.
  *
  * @author Frederic Camblor, BDX.IO 2014
  */


// USAGE: Used only inside this file
case class ConferenceUrls(registration: String,confWebsite: String, cfpHostname: String, schedule: String){
    def cfpURL:String={
    if(Play.current.configuration.getBoolean("cfp.activateHTTPS").getOrElse(false)){
      s"https://$cfpHostname"
    }else{
      s"http://$cfpHostname"
    }
  }

}


// USAGE: used only inside this file
case class ConferenceTiming(
                             datesI18nKey: String,
                             datesEn: String,
                             cfpOpenedOn: LocalDate,
                             cfpClosedOn: LocalDate,
                             scheduleAnnouncedOn: LocalDate,
                             days:Iterator[LocalDate]
                           )
//USAGE: only inside this file
case class ConferenceSponsor(showSponsorProposalCheckbox: Boolean, sponsorProposalType: ProposalType = ProposalType.UNKNOWN)


//USAGE inside this file and returned in parse which is used by ApprovedProposal
case class ProposalConfiguration(id: String, slotsCount: Int,
                                 givesSpeakerFreeEntrance: Boolean,
                                 freeEntranceDisplayed: Boolean,
                                 htmlClass: String,
                                 hiddenInCombo: Boolean = false,
                                 chosablePreferredDay: Boolean = false,
                                 impliedSelectedTrack: Option[Track] = None)
//USAGE: used in many places
object ProposalConfiguration {

  val UNKNOWN = ProposalConfiguration(id = "unknown", slotsCount = 0, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false,
    htmlClass = "", hiddenInCombo = true, chosablePreferredDay = false)

  def parse(propConf: String): ProposalConfiguration = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.find(p => p.id == propConf).getOrElse(ProposalConfiguration.UNKNOWN)
  }

  def totalSlotsCount = ConferenceDescriptor.ConferenceProposalConfigurations.ALL.map(_.slotsCount).sum

  def isDisplayedFreeEntranceProposals(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.freeEntranceDisplayed).headOption.getOrElse(false)
  }

  def getProposalsImplyingATrackSelection = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.impliedSelectedTrack.nonEmpty)
  }

  def getHTMLClassFor(pt: ProposalType): String = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.htmlClass).headOption.getOrElse("unknown")
  }

  def isChosablePreferredDaysProposals(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.chosablePreferredDay).headOption.getOrElse(false)
  }

  def doesProposalTypeGiveSpeakerFreeEntrance(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.givesSpeakerFreeEntrance).headOption.getOrElse(false)
  }
}

case class ConferenceNaming(title:String, shortTitle:String) {
  private val longSplittedName = shortTitle.split("(\\s)+")
  val longName = s"$shortTitle CFP"
  val longSplittedName_whiteStart = longSplittedName(0)
  val longSplittedName_colored = longSplittedName(1)
  val longSplittedName_whiteEnd = longSplittedName(2)
}

case class ConferenceDescriptor(eventCode: String,
                                fromEmail: String,
                                committeeEmail: String,
                                bccEmail: Option[String],
                                bugReportRecipient: String,
                                conferenceUrls: ConferenceUrls,
                                timing: ConferenceTiming,
                                hosterName: String,
                                hosterWebsite: String,
                                hashTag: String,
                                conferenceSponsor: ConferenceSponsor,
                                locale: List[Locale],
                                localisation: String,
                                maxProposalSummaryCharacters:Int=1200,
                                trackleadersEmail: String,
                                naming: ConferenceNaming,
                                startDate: Date,
                                endDate: Date,
                                isCfpOpen: Boolean
                               )

object ConferenceDescriptor {

  /**
    * TODO configure here the kind of talks you will propose
    */
  object ConferenceProposalTypes {

    val CONF = ProposalType(id = "pbig", label = "palestradebate.label")
    val BOF = ProposalType(id = "pmeia", label = "meiapalestra.label")
    val QUICK = ProposalType(id = "pmini", label = "minipalestra.label")
    val IGNITE= ProposalType(id = "prel", label = "relampago.label")
    val ONLINE = ProposalType(id = "ponline", label = "palestraonline.label")

    val ALL = List(CONF, BOF, QUICK)

    def valueOf(id: String): ProposalType = id match {
      case "pbig" => CONF
      case "pmeia" => BOF
      case "pmini" => QUICK
      //case "prel" => IGNITE
      //case "ponline" => ONLINE
    }

  }

  // TODO Configure here the slot, with the number of slots available, if it gives a free ticket to the speaker, some CSS icons
  object ConferenceProposalConfigurations {

// ===================================== slotsCount doesn't apply to TDC
    val CONF = ProposalConfiguration(id = "pbig", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-group",
      chosablePreferredDay = true)
    val BOF = ProposalConfiguration(id = "pmeia", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
    val QUICK = ProposalConfiguration(id = "pmini", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-fast-forward",
      chosablePreferredDay = true)
    val IGNITE = ProposalConfiguration(id = "prel", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-fast-forward",
      chosablePreferredDay = true)
    val ONLINE = ProposalConfiguration(id = "ponline", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
      chosablePreferredDay = true)

    val ALL = List(CONF, BOF, QUICK )
  }

  // Configure here all your Conference's tracks.
  object ConferenceTracks {
    def ALL = Track.allTracks().sortBy(_.id)
  }

  // TODO If you want to use the Devoxx Scheduler, you can describe here the list of rooms, with capacity for seats
  object ConferenceRooms {
    val allRooms = List[Room]()
  }


  // TODO The idea here is to describe in term of Agenda, for each rooms, the slots. This is required only for the Scheduler
  object ConferenceSlots {

    // Registration, coffee break, lunch etc
    val wednesdayBreaks = List[Slot]()

    val thursdayBreaks = List[Slot]()

    val fridayBreaks = List[Slot]()

    val wednesday: List[Slot] = {
      wednesdayBreaks
    }

    val thursday: List[Slot] = {
      thursdayBreaks
    }

    val friday: List[Slot] = {
      fridayBreaks
    }

    // COMPLETE DEVOXX
    def all: List[Slot] = {
      wednesday ++ thursday ++ friday
    }
  }

  def dateRange(from: LocalDate, to: LocalDate, step: Period): Iterator[LocalDate]      =Iterator.iterate(from)(_.plus(step)).takeWhile(!_.isAfter(to))

  private var selectedDescriptor:Option[ConferenceDescriptor] = None
  
  import scala.collection.mutable.Map
  private val conferenceCache:Map[String,ConferenceDescriptor] = Map.empty
  
  def current() = {
    if(selectedDescriptor.isEmpty) {
      selectConference("TDC2019POA")
    } 
    selectedDescriptor.get
  }

  /**
   * changes the current active conference
   */
  def selectConference(event:String):Unit = {
    if(conferenceCache.isDefinedAt(event)) {
      selectedDescriptor = Option(conferenceCache(event))
    } else {
      val conference = TDCConference.load(event).getOrElse(defaultConference)
      val newDescriptor = ConferenceDescriptor(
        eventCode = conference.eventCode,
        fromEmail = Play.current.configuration.getString("mail.from").getOrElse("organizacao@thedevelopersconference.com.br"),
        committeeEmail = Play.current.configuration.getString("mail.committee.email").getOrElse("organizacao@thedevelopersconference.com.br"),
        bccEmail = Play.current.configuration.getString("mail.bcc"),
        trackleadersEmail = Play.current.configuration.getString("mail.trackleaders.email").getOrElse("coordenadores@thedevelopersconference.com.br"),
        bugReportRecipient = Play.current.configuration.getString("mail.bugreport.recipient").getOrElse("cfp@thedevelopersconference.com.br"),
        conferenceUrls = ConferenceUrls(
          registration = conference.registrationUrl,
          confWebsite = "http:/thedevconf.com.br",
          cfpHostname = Play.current.configuration.getString("cfp.hostname").getOrElse("cfp-poa.thedevconf.com.br"),
          schedule = conference.scheduleUrl
        ),
        timing = ConferenceTiming(
          datesI18nKey = Messages("CONF.dates",conference.startDate.toDate(),conference.endDate.toDate()),
          datesEn = Messages("CONF.dates",conference.startDate.toDate(),conference.endDate.toDate())(Lang("en")),
          cfpOpenedOn = conference.cfpOpenDate,
          cfpClosedOn = conference.cfpCloseDate,
          scheduleAnnouncedOn = conference.scheduleAnnouncedOn,
          days = dateRange(conference.startDate, conference.endDate, new Period().withDays(1))
        ),
        hosterName = "AWS", hosterWebsite = "http://aws.amazon.com/",
        hashTag = "#TheDevConf",
        conferenceSponsor = ConferenceSponsor(showSponsorProposalCheckbox = false, sponsorProposalType = ConferenceProposalTypes.CONF),
        locale = List(new Locale("pt", "BR")),
        localisation = conference.localisation,
        maxProposalSummaryCharacters = 700,
        naming = ConferenceNaming(
          title = conference.title,
          shortTitle = conference.shortTitle
        ),
        startDate = conference.startDate.toDate(),
        endDate = conference.endDate.toDate(),
        isCfpOpen = conference.cfpOpen.getOrElse(false)
      )
      selectedDescriptor = Option(newDescriptor)
      conferenceCache += ( event -> newDescriptor )
    }
  }
  
  // It has to be a def, not a val, else it is not re-evaluated
  def isCFPOpen: Boolean = {
    current().isCfpOpen
  }

  /**
   * removes the selected conference configuration from the conference cache
   */
   def clearCache(eventCode:String) = {
      selectedDescriptor = None
      conferenceCache -= eventCode
   }

  def isGoldenTicketActive:Boolean = Play.current.configuration.getBoolean("goldenTicket.active").getOrElse(false)

  def isFavoritesSystemActive:Boolean = Play.current.configuration.getBoolean("cfp.activateFavorites").getOrElse(false)

  def isHTTPSEnabled = Play.current.configuration.getBoolean("cfp.activateHTTPS").getOrElse(false)

  private val defaultConference = TDCConference(
    eventCode = "",
    title = "",
    shortTitle = "TDC 2019 POA",
    localisation = "",
    cfpOpenDate = LocalDate.now(),
    cfpCloseDate = LocalDate.now(),
    scheduleAnnouncedOn = LocalDate.now(),
    startDate= LocalDate.now(),
    endDate= LocalDate.now(),
    registrationUrl = "",
    scheduleUrl = ""
  )
}


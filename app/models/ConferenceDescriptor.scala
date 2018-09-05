package models

import java.util.Locale

import org.joda.time.{Period, DateTime, DateTimeZone}
import play.api.Play

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

case class ConferenceUrls(faq: String, registration: String,confWebsite: String, cfpHostname: String){
    def cfpURL:String={
    if(Play.current.configuration.getBoolean("cfp.activateHTTPS").getOrElse(false)){
      s"https://$cfpHostname"
    }else{
      s"http://$cfpHostname"
    }
  }

}

case class ConferenceTiming(
                             datesI18nKey: String,
                             speakersPassDuration: Integer,
                             preferredDayEnabled: Boolean,
                             firstDayFr: String,
                             firstDayEn: String,
                             datesFr: String,
                             datesEn: String,
                             cfpOpenedOn: DateTime,
                             cfpClosedOn: DateTime,
                             scheduleAnnouncedOn: DateTime,
                             days:Iterator[DateTime]
                           )

case class ConferenceSponsor(showSponsorProposalCheckbox: Boolean, sponsorProposalType: ProposalType = ProposalType.UNKNOWN)

case class TrackDesc(id: String, imgSrc: String, i18nTitleProp: String, i18nDescProp: String)

case class ProposalConfiguration(id: String, slotsCount: Int,
                                 givesSpeakerFreeEntrance: Boolean,
                                 freeEntranceDisplayed: Boolean,
                                 htmlClass: String,
                                 hiddenInCombo: Boolean = false,
                                 chosablePreferredDay: Boolean = false,
                                 impliedSelectedTrack: Option[Track] = None)

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

case class ConferenceDescriptor(eventCode: String,
                                confUrlCode: String,
                                frLangEnabled: Boolean,
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
                                notifyProposalSubmitted:Boolean,
                                maxProposalSummaryCharacters:Int=1200
                               )

object ConferenceDescriptor {

  /**
    * TODO configure here the kind of talks you will propose
    */
  object ConferenceProposalTypes {

  
   // val CONF = ProposalType(id = "conf", label = "conf.label")

    val UNI = ProposalType(id = "uni", label = "uni.label")

    val TIA = ProposalType(id = "tia", label = "tia.label")

    val LAB = ProposalType(id = "lab", label = "lab.label")

   // val QUICK = ProposalType(id = "quick", label = "quick.label")

   // val BOF = ProposalType(id = "bof", label = "bof.label")

    val KEY = ProposalType(id = "key", label = "key.label")

    // val IGNITE = ProposalType(id = "ignite", label = "ignite.label")

    val OTHER = ProposalType(id = "other", label = "other.label")

 
    val CONF = ProposalType(id = "pbig", label = "palestradebate.label")

    val BOF = ProposalType(id = "pmeia", label = "meiapalestra.label")

    val QUICK = ProposalType(id = "pmini", label = "minipalestra.label")

    val IGNITE= ProposalType(id = "prel", label = "relampago.label")

    val ALL = List(CONF, BOF, QUICK)

    def valueOf(id: String): ProposalType = id match {

 /******************************* comentando os tipos de propostas originais do devoxx
      case "conf" => CONF
      case "uni" => UNI
      case "tia" => TIA
      case "lab" => LAB
      case "quick" => QUICK
      case "bof" => BOF
      case "key" => KEY
      case "ignite" => IGNITE
      case "other" => OTHER
*/

      case "pbig" => CONF
      case "pmeia" => BOF
      case "pmini" => QUICK
      //case "prel" => IGNITE
    }

  }

  // TODO Configure here the slot, with the number of slots available, if it gives a free ticket to the speaker, some CSS icons
  object ConferenceProposalConfigurations {
 //   val CONF = ProposalConfiguration(id = "conf", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.CONF.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
 //     chosablePreferredDay = true)
    val UNI = ProposalConfiguration(id = "uni", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.UNI.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-laptop",
      chosablePreferredDay = true)
    val TIA = ProposalConfiguration(id = "tia", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.TIA.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-legal",
      chosablePreferredDay = true)
    val LAB = ProposalConfiguration(id = "lab", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.LAB.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-beaker",
      chosablePreferredDay = true)
//    val QUICK = ProposalConfiguration(id = "quick", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.QUICK.id)), givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-fast-forward",
//      chosablePreferredDay = true)
//    val BOF = ProposalConfiguration(id = "bof", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.BOF.id)), givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-group",
//      chosablePreferredDay = false)
    val KEY = ProposalConfiguration(id = "key", slotsCount = 7, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
//    val IGNITE = ProposalConfiguration(id = "ignite", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.IGNITE.id)), givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
//      chosablePreferredDay = false)
    val OTHER = ProposalConfiguration(id = "other", slotsCount = 5, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      hiddenInCombo = true, chosablePreferredDay = false)
// ===================================== slotsCount doesn't apply to TDC
    val CONF = ProposalConfiguration(id = "pbig", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-group",
      chosablePreferredDay = true)
    val BOF = ProposalConfiguration(id = "pmeia", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
    val QUICK = ProposalConfiguration(id = "pmini", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
    val IGNITE = ProposalConfiguration(id = "prel", slotsCount = 0, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-fast-forward",
      chosablePreferredDay = true)

    val ALL = List(CONF, BOF, QUICK )

    def doesItGivesSpeakerFreeEntrance(proposalType: ProposalType): Boolean = {
      ALL.filter(_.id == proposalType.id).exists(_.givesSpeakerFreeEntrance)
    }
  }

  // Configure here all your Conference's tracks.
  object ConferenceTracks {

    val DOTNET = Track("dotnet", "dotnet.label")
    val ACESSIBILIDADE = Track("acessibilidade", "acessibilidade.label")
    val AGILE = Track("agile", "agile.label")
    val AGILECOACH = Track("agilecoach", "agilecoach.label")
    val ANALISE = Track("analise", "analise.label")
    val ANDROID = Track("android", "android.label")
    val ARDUINO = Track("arduino", "arduino.label")
    val ARQUITCORP = Track("arquitetcorp", "arquitetcorp.label")
    val ARQUITDADOS = Track("arquitetdados", "arquitetdados.label")
    val ARQUITJAVA = Track("arquitetjava", "arquitetjava.label")
    val ARQUITNET = Track("arquitetnet", "arquitetnet.label")

    val BEGINNERS = Track("beginners", "beginners.label")
    val BIGDATA = Track("bigdata", "bigdata.label")
    val BLOCKCHAIN = Track("blockchain", "blockchain.label")

    val CARREIRA = Track("carreira", "carreira.label")
    val CHATBOTS = Track("chatbots", "chatbots.label")
    val CLOUD = Track("cloud", "cloud.label")
    val CONTAINERS = Track("containers", "containers.label")

    val DATASCIENCE = Track("datascience", "datascience.label")
    val DTHINKING = Track("dthinking", "dthinking.label")
    val DEVOPS = Track("devops", "devops.label")
    val DEVTEST = Track("devtest", "devtest.label")

    val GESTAOPROD = Track("gestaoprod", "gestaoprod.label")
    val GAMES = Track("games", "games.label")
    val GOLANG = Track("golang", "golang.label")

    val INOVACAO = Track("inovacao", "inovacao.label")
    val INTELARTIF = Track("intelartif", "intelartif.label")
    val IOS = Track("ios", "ios.label")
    val IOT = Track("iot", "iot.label")

    val JAVA = Track("java", "java.label")
    val JAVASCRIPT = Track("javascript", "javascript.label")

    val MACHINE = Track("machine", "machine.label")
    val MANAGEMENT = Track("management", "management.label")
    val MICROSERVICES = Track("microservices", "microservices.label")

    val NODEJS = Track("nodejs", "nodejs.label")
    val NOSQL = Track("nosql", "nosql.label")

    val PHP = Track("php", "php.label")
    val PYTHON = Track("python", "python.label")

    val REALIDADE = Track("realidade", "realidade.label")

    val SEGURANCA = Track("seguranca", "seguranca.label")

    val TESTES = Track("testes", "testes.label")
    val TRANSFDIGITAL = Track("transfdigital", "transfdigital.label")
    val UXDESIGN = Track("uxdesign", "uxdesign.label")
    val XP = Track("xp", "xp.label")
    val WEB = Track("web", "web.label")
    val WOMEN = Track("women", "women.label")

    val ALL = List(DOTNET, ACESSIBILIDADE, AGILE, AGILECOACH, ANALISE, ANDROID, ARDUINO, ARQUITCORP, ARQUITDADOS,
      ARQUITJAVA, ARQUITNET, BEGINNERS, BIGDATA, BLOCKCHAIN, CARREIRA, CHATBOTS, CLOUD, CONTAINERS, DATASCIENCE,
      DTHINKING, DEVTEST, DEVOPS, GAMES, GESTAOPROD, GOLANG, INOVACAO, INTELARTIF, IOS, IOT, JAVA, JAVASCRIPT, MACHINE,
      MANAGEMENT, MICROSERVICES, NODEJS, NOSQL, PHP, PYTHON, REALIDADE, TESTES, TRANSFDIGITAL, UXDESIGN, XP, WEB, WOMEN)

  }

  // TODO configure the description for each Track
  object ConferenceTracksDescription {

    val DOTNET = TrackDesc(ConferenceTracks.DOTNET.id, "/assets/tdc2016poa/images/icon_dotnet.png", "track.dotnet.title", "track.dotnet.desc")
    val ACESSIBILIDADE = TrackDesc(ConferenceTracks.ACESSIBILIDADE.id, "/assets/tdc2016poa/images/icon_acessibilidade.png", "track.acessibilidade.title", "track.acessibilidade.desc")
    val AGILE = TrackDesc(ConferenceTracks.AGILE.id, "/assets/tdc2016poa/images/icon_agile.png", "track.agile.title", "track.agile.desc")
    val AGILECOACH = TrackDesc(ConferenceTracks.AGILECOACH.id, "/assets/tdc2016poa/images/icon_agilecoach.png", "track.agilecoach.title", "track.agilecoach.desc")
    val ANALISE = TrackDesc(ConferenceTracks.ANALISE.id, "/assets/tdc2016poa/images/icon_analise.png", "track.analise.title", "track.analise.desc")
    val ANDROID = TrackDesc(ConferenceTracks.ANDROID.id, "/assets/tdc2016poa/images/icon_android.png", "track.android.title", "track.android.desc")
    val ARDUINO = TrackDesc(ConferenceTracks.ARDUINO.id, "/assets/tdc2016poa/images/icon_arduino.png", "track.arduino.title", "track.arduino.desc")
    val ARQUITCORP = TrackDesc(ConferenceTracks.ARQUITCORP.id, "/assets/tdc2016poa/images/icon_arquitetcorp.png", "track.arquitetcorp.title", "track.arquitetcorp.desc")
    val ARQUITDADOS = TrackDesc(ConferenceTracks.ARQUITDADOS.id, "/assets/tdc2016poa/images/icon_arquitetdados.png", "track.arquitetdados.title", "track.arquitetdados.desc")
    val ARQUITJAVA = TrackDesc(ConferenceTracks.ARQUITJAVA.id, "/assets/tdc2016poa/images/icon_arquitetjava.png", "track.arquitetjava.title", "track.arquitetjava.desc")
    val ARQUITNET = TrackDesc(ConferenceTracks.ARQUITNET.id, "/assets/tdc2016poa/images/icon_arquitetnet.png", "track.arquitetnet.title", "track.arquitetnet.desc")

    val BEGINNERS = TrackDesc(ConferenceTracks.BEGINNERS.id, "/assets/tdc2016poa/images/icon_bigdata.png", "track.beginners.title", "track.beginners.desc")
    val BIGDATA = TrackDesc(ConferenceTracks.BIGDATA.id, "/assets/tdc2016poa/images/icon_bigdata.png", "track.bigdata.title", "track.bigdata.desc")
    val BLOCKCHAIN = TrackDesc(ConferenceTracks.BLOCKCHAIN.id, "/assets/tdc2016poa/images/icon_bigdata.png", "track.blockchain.title", "track.blockchain.desc")

    val CARREIRA = TrackDesc(ConferenceTracks.CARREIRA.id, "/assets/tdc2016poa/images/icon_carreira.png", "track.carreira.title", "track.carreira.desc")
    val CHATBOTS = TrackDesc(ConferenceTracks.CHATBOTS.id, "/assets/tdc2016poa/images/icon_chatbots.png", "track.chatbots.title", "track.chatbots.desc")
    val CLOUD = TrackDesc(ConferenceTracks.CLOUD.id, "/assets/tdc2016poa/images/icon_cloud.png", "track.cloud.title", "track.cloud.desc")
    val CONTAINERS = TrackDesc(ConferenceTracks.CLOUD.id, "/assets/tdc2016poa/images/icon_cloud.png", "track.containers.title", "track.containers.desc")

    val DATASCIENCE = TrackDesc(ConferenceTracks.DATASCIENCE.id, "/assets/tdc2016poa/images/icon_datascience.png", "track.datascience.title", "track.datascience.desc")
    val DEVOPS = TrackDesc(ConferenceTracks.DEVOPS.id, "/assets/tdc2016poa/images/icon_devops.png", "track.devops.title", "track.devops.desc")
    val DEVTEST = TrackDesc(ConferenceTracks.DEVTEST.id, "/assets/tdc2016poa/images/icon_devtest.png", "track.devtest.title", "track.devtest.desc")
    val DTHINKING = TrackDesc(ConferenceTracks.DTHINKING.id, "/assets/tdc2016poa/images/icon_dthinking.png", "track.dthinking.title", "track.dthinking.desc")

    val GESTAOPROD = TrackDesc(ConferenceTracks.GESTAOPROD.id, "/assets/tdc2016poa/images/icon_gestaoprod.png", "track.gestaoprod.title", "track.gestaoprod.desc")
    val GAMES = TrackDesc(ConferenceTracks.GAMES.id, "/assets/tdc2016poa/images/icon_games.png", "track.games.title", "track.games.desc")
    val GOLANG = TrackDesc(ConferenceTracks.GOLANG.id, "/assets/tdc2016poa/images/icon_golang.png", "track.golang.title", "track.golang.desc")

    val INOVACAO = TrackDesc(ConferenceTracks.INOVACAO.id, "/assets/tdc2016poa/images/icon_inovacao.png", "track.inovacao.title", "track.inovacao.desc")
    val INTELARTIF = TrackDesc(ConferenceTracks.INTELARTIF.id, "/assets/tdc2016poa/images/icon_intelartif.png", "track.intelartif.title", "track.intelartif.desc")
    val IOS = TrackDesc(ConferenceTracks.IOS.id, "/assets/tdc2016poa/images/icon_ios.png", "track.ios.title", "track.ios.desc")
    val IOT = TrackDesc(ConferenceTracks.IOT.id, "/assets/tdc2016poa/images/icon_iot.png", "track.iot.title", "track.iot.desc")

    val JAVA = TrackDesc(ConferenceTracks.JAVA.id, "/assets/tdc2016poa/images/icon_java.png", "track.java.title", "track.java.desc")
    val JAVASCRIPT = TrackDesc(ConferenceTracks.JAVASCRIPT.id, "/assets/tdc2016poa/images/icon_javascript.png", "track.javascript.title", "track.javascript.desc")

    val MACHINE = TrackDesc(ConferenceTracks.MACHINE.id, "/assets/tdc2016poa/images/icon_machine.png", "track.machine.title", "track.machine.desc")
    val MANAGEMENT = TrackDesc(ConferenceTracks.MANAGEMENT.id, "/assets/tdc2016poa/images/icon_management.png", "track.management.title", "track.management.desc")
    val MICROSERVICES = TrackDesc(ConferenceTracks.MICROSERVICES.id, "/assets/tdc2016poa/images/icon_bd.png", "track.microservices.title", "track.microservices.desc")

    val NODEJS = TrackDesc(ConferenceTracks.NODEJS.id, "/assets/tdc2016poa/images/icon_nodejs.png", "track.nodejs.title", "track.nodejs.desc")
    val NOSQL = TrackDesc(ConferenceTracks.NOSQL.id, "/assets/tdc2016poa/images/icon_nosql.png", "track.nosql.title", "track.nosql.desc")

    val PHP = TrackDesc(ConferenceTracks.PHP.id, "/assets/tdc2016poa/images/icon_php.png", "track.php.title", "track.phpessencial.desc")
    val PYTHON = TrackDesc(ConferenceTracks.PYTHON.id, "/assets/tdc2016poa/images/icon_python.png", "track.python.title", "track.python.desc")

    val REALIDADE = TrackDesc(ConferenceTracks.REALIDADE.id, "/assets/tdc2016poa/images/icon_realidade.png", "track.realidade.title", "track.realidade.desc")

    val SEGURANCA = TrackDesc(ConferenceTracks.SEGURANCA.id, "/assets/tdc2016poa/images/icon_seguranca.png", "track.seguranca.title", "track.seguranca.desc")

    val TESTES = TrackDesc(ConferenceTracks.TESTES.id, "/assets/tdc2016poa/images/icon_testes.png", "track.testes.title", "track.testes.desc")
    val TRANSFDIGITAL = TrackDesc(ConferenceTracks.TRANSFDIGITAL.id, "/assets/tdc2016poa/images/icon_transfdigital.png", "track.transfdigital.title", "track.transfdigital.desc")
    val UXDESIGN = TrackDesc(ConferenceTracks.UXDESIGN.id, "/assets/tdc2016poa/images/icon_uxdesign.png", "track.uxdesign.title", "track.uxdesign.desc")
    val WEB = TrackDesc(ConferenceTracks.WEB.id, "/assets/tdc2016poa/images/icon_web.png", "track.web.title", "track.web.desc")
    val WOMEN = TrackDesc(ConferenceTracks.WOMEN.id, "/assets/tdc2016poa/images/icon_women.png", "track.women.title", "track.women.desc")
    val XP = TrackDesc(ConferenceTracks.XP.id, "/assets/tdc2016poa/images/icon_xp.png", "track.xp.title", "track.xp.desc")

    val ALL = List(DOTNET, ACESSIBILIDADE, AGILE, AGILECOACH, ANALISE, ANDROID, ARDUINO, ARQUITCORP, ARQUITDADOS,
      ARQUITJAVA, ARQUITNET, BEGINNERS, BIGDATA, BLOCKCHAIN, CARREIRA, CHATBOTS, CLOUD, CONTAINERS, DATASCIENCE,
      DTHINKING, DEVTEST, DEVOPS, GAMES, GESTAOPROD, GOLANG, INOVACAO, INTELARTIF, IOS, IOT, JAVA, JAVASCRIPT, MACHINE,
      MANAGEMENT, MICROSERVICES, NODEJS, NOSQL, PHP, PYTHON, REALIDADE, TESTES, TRANSFDIGITAL, UXDESIGN, XP, WEB, WOMEN)

    def findTrackDescFor(t: Track): TrackDesc = {
      ALL.find(_.id == t.id).getOrElse(DOTNET)
    }
  }

  // TODO If you want to use the Devoxx Scheduler, you can describe here the list of rooms, with capacity for seats
  object ConferenceRooms {

    // Tip : I use the ID to sort-by on the view per day... So if the exhibition floor id is "aaa" it will be
    // the first column on the HTML Table

    // Do not change the ID's once the program is published
    val HALL_EXPO = Room("a_hall", "Exhibition floor", 2300, "special", "")
    val HALL_A = Room("x_hall_a", "Open Data Camp", 100, "special", "")

    val AMPHI_BLEU = Room("b_amphi", "Amphi Bleu", 826, "theatre", "camera")
    val MAILLOT = Room("c_maillot", "Maillot", 380, "theatre", "camera")
    val NEUILLY_251 = Room("f_neu251", "Neuilly 251", 220, "theatre", "camera")
    val NEUILLY_252AB = Room("e_neu252", "Neuilly 252 AB", 380, "theatre", "camera")
    val NEUILLY_253 = Room("neu253", "Neuilly 253 Lab", 60, "classroom", "rien")
    val NEUILLY_253_T = Room("neu253_t", "Neuilly 253", 120, "theatre", "son")

    val PARIS_241 = Room("d_par241", "Paris 241", 220, "theatre", "camera")
    val PARIS_242AB_T = Room("par242AB", "Paris 242-AB", 280, "theatre", "camera")
    val PARIS_242A = Room("par242A", "Paris 242A Lab", 60, "classroom", "rien")
    val PARIS_242B = Room("par242B", "Paris 242B Lab", 60, "classroom", "rien")
    val PARIS_242A_T = Room("par242AT", "Paris 242A", 120, "theatre", "rien")
    val PARIS_242B_T = Room("par242BT", "Paris 242B", 120, "theatre", "rien")

    val PARIS_243 = Room("par243", "Paris 243", 60, "classroom", "rien")
    val PARIS_243_T = Room("par243_t", "Paris 243", 120, "theatre", "son")

    val PARIS_202_203 = Room("par202_203", "Paris 202-203 Lab", 32, "classroom", "rien")
    val PARIS_221M_222M = Room("par221M-222M", "Paris 221M-222M Lab", 32, "classroom", "rien")
    val PARIS_224M_225M = Room("par224M-225M", "Paris 224M-225M Lab", 26, "classroom", "rien")

    val NEUILLY_212_213 = Room("neu_212_213", "Neuilly 212-213M Lab", 32, "classroom", "rien")
    val NEUILLY_231_232 = Room("neu_232_232", "Neuilly 231-232M Lab", 32, "classroom", "rien")
    val NEUILLY_234_235 = Room("neu_234_235", "Neuilly 234_234M Lab", 32, "classroom", "rien")

    val PARIS_204 = Room("par204", "Paris 204", 16, "classroom", "rien")
    val PARIS_201 = Room("par201", "Paris 201", 14, "classroom", "rien")

    val ROOM_OTHER = Room("other_room", "Autres salles", 100, "classroom", "rien")

    val allRooms = List(HALL_EXPO, HALL_A, AMPHI_BLEU, MAILLOT, PARIS_241, NEUILLY_251, NEUILLY_252AB,
      PARIS_242A, PARIS_242B, PARIS_243, PARIS_243_T, NEUILLY_253, NEUILLY_253_T,
      PARIS_202_203, PARIS_221M_222M, PARIS_224M_225M, PARIS_204, PARIS_201)

    val allRoomsUni = List(AMPHI_BLEU, MAILLOT, NEUILLY_251, PARIS_241,NEUILLY_252AB)

    val allRoomsTIAWed = allRoomsUni ++ List(PARIS_242A_T, PARIS_242B_T, PARIS_243, NEUILLY_253)
    val allRoomsTIAThu = allRoomsUni ++ List(PARIS_242AB_T, PARIS_243_T, NEUILLY_253_T)

    val allRoomsLabsWednesday = List(PARIS_242A, PARIS_242B, PARIS_243, NEUILLY_253) ++ List(PARIS_202_203, PARIS_221M_222M, PARIS_224M_225M,  NEUILLY_231_232, NEUILLY_234_235)
    val allRoomsLabThursday = List(PARIS_202_203, PARIS_221M_222M, PARIS_224M_225M)
    val allRoomsLabFriday = List(PARIS_202_203, PARIS_221M_222M, PARIS_224M_225M, NEUILLY_212_213, NEUILLY_231_232, NEUILLY_234_235 )

    val allRoomsBOF = List(PARIS_242AB_T, NEUILLY_252AB, PARIS_241, NEUILLY_251, PARIS_243_T, NEUILLY_253_T, PARIS_202_203, PARIS_221M_222M, PARIS_224M_225M)

    val keynoteRoom = List(AMPHI_BLEU)

    val allRoomsConf = List(AMPHI_BLEU, MAILLOT, PARIS_242AB_T, NEUILLY_252AB, PARIS_241, NEUILLY_251, PARIS_243_T, NEUILLY_253_T)

    val allRoomsQuickiesThu = allRoomsConf.filterNot(r => r.id == AMPHI_BLEU.id || r.id == MAILLOT.id) ++ List(PARIS_202_203) // Retire les Ignites
    val allRoomsQuickiesFriday = allRoomsConf.filterNot(r => r.id == AMPHI_BLEU.id || r.id == MAILLOT.id)
  }


  // TODO if you want to use the Scheduler, you can configure the breaks
  object ConferenceSlotBreaks {
    val registration = SlotBreak("reg", "Registration, Welcome and Breakfast", "Accueil", ConferenceRooms.HALL_EXPO)
    val petitDej = SlotBreak("dej", "Breakfast", "Accueil et petit-déjeuner", ConferenceRooms.HALL_EXPO)
    val coffee = SlotBreak("coffee", "Coffee Break", "Pause café", ConferenceRooms.HALL_EXPO)
    val lunch = SlotBreak("lunch", "Lunch", "Pause déjeuner", ConferenceRooms.HALL_EXPO)
    val shortBreak = SlotBreak("chgt", "Break", "Pause courte", ConferenceRooms.HALL_EXPO)
    val exhibition = SlotBreak("exhib", "Exhibition", "Exhibition", ConferenceRooms.HALL_EXPO)
    val meetAndGreet = SlotBreak("meet", "Meet & Greet", "Exhibition", ConferenceRooms.HALL_EXPO)
  }

  // TODO The idea here is to describe in term of Agenda, for each rooms, the slots. This is required only for the Scheduler
  object ConferenceSlots {

    // UNIVERSITY
    val universitySlotsWednesday: List[Slot] = {
      val universityWednesdayMorning = ConferenceRooms.allRoomsUni.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.UNI.id, "wednesday",
            new DateTime("2016-04-20T09:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val universityWednesdayAfternoon = ConferenceRooms.allRoomsUni.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.UNI.id, "wednesday",
            new DateTime("2016-04-20T13:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T16:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      universityWednesdayMorning ++ universityWednesdayAfternoon
    }

    // TOOLS IN ACTION
    val tiaSlotsWednesday: List[Slot] = {

      val toolsWednesdayAfternoonSlot1 = ConferenceRooms.allRoomsTIAWed.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.TIA.id, "wednesday",
            new DateTime("2016-04-20T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T17:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val toolsWednesdayAfternoonSlot2 = ConferenceRooms.allRoomsTIAWed.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.TIA.id, "wednesday",
            new DateTime("2016-04-20T17:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T18:25:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      val toolsWednesdayAfternoonSlot3 = ConferenceRooms.allRoomsTIAWed.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.TIA.id, "wednesday",
            new DateTime("2016-04-20T18:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T19:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r3)
      }
      toolsWednesdayAfternoonSlot1 ++ toolsWednesdayAfternoonSlot2 ++ toolsWednesdayAfternoonSlot3
    }

    val tiaSlotsThursday: List[Slot] = {
      val toolsThursdayAfternoonSlot1 = ConferenceRooms.allRoomsTIAThu.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.TIA.id, "thursday",
            new DateTime("2016-04-21T18:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T18:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val toolsThursdayAfternoonSlot2 = ConferenceRooms.allRoomsTIAThu.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.TIA.id, "thursday",
            new DateTime("2016-04-21T18:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T19:25:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      toolsThursdayAfternoonSlot1 ++ toolsThursdayAfternoonSlot2
    }

    // HANDS ON LABS
    val labsSlotsWednesday: List[Slot] = {
      val labsWednesdayMorning = ConferenceRooms.allRoomsLabsWednesday.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.LAB.id, "wednesday",
            new DateTime("2016-04-20T09:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val labsWednesdayAfternoon = ConferenceRooms.allRoomsLabsWednesday.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.LAB.id, "wednesday",
            new DateTime("2016-04-20T13:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-20T16:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      labsWednesdayMorning ++ labsWednesdayAfternoon
    }

    val labsSlotsThursday: List[Slot] = {

      val labsThursdayMorning = ConferenceRooms.allRoomsLabThursday.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.LAB.id, "thursday",
            new DateTime("2016-04-21T12:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T15:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val labsThursdayAfternoon = ConferenceRooms.allRoomsLabThursday.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.LAB.id, "thursday",
            new DateTime("2016-04-21T16:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T19:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      labsThursdayMorning ++ labsThursdayAfternoon
    }

    val labsSlotsFriday: List[Slot] = {

      val labsFridayMorning = ConferenceRooms.allRoomsLabFriday.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.LAB.id, "friday",
            new DateTime("2016-04-22T11:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      labsFridayMorning
    }

    // BOFS
    val bofSlotsThursday: List[Slot] = {

      val bofThursdayEveningSlot1 = ConferenceRooms.allRoomsBOF.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.BOF.id, "thursday",
            new DateTime("2016-04-21T19:45:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T20:35:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val bofThursdayEveningSlot2 = ConferenceRooms.allRoomsBOF.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.BOF.id, "thursday",
            new DateTime("2016-04-21T20:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T21:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      val bofThursdayEveningSlot3 = ConferenceRooms.allRoomsBOF.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.BOF.id, "thursday",
            new DateTime("2016-04-21T21:35:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T22:25:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r3)
      }
      bofThursdayEveningSlot1 ++ bofThursdayEveningSlot2 ++ bofThursdayEveningSlot3
    }

    // QUICKIES
    val quickiesSlotsThursday: List[Slot] = {
      val quickiesThursdayLunch1 = ConferenceRooms.allRoomsQuickiesThu.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "thursday",
            new DateTime("2016-04-21T12:05:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T12:20:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val quickiesThursdayLunch2 = ConferenceRooms.allRoomsQuickiesThu.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "thursday",
            new DateTime("2016-04-21T12:25:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T12:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      quickiesThursdayLunch1 ++ quickiesThursdayLunch2
    }

    val quickiesSlotsFriday: List[Slot] = {
      val quickFriday1 = ConferenceRooms.allRoomsQuickiesFriday.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "friday",
            new DateTime("2016-04-22T12:05:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T12:20:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val quickFriday2 = ConferenceRooms.allRoomsQuickiesFriday.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "friday",
            new DateTime("2016-04-22T12:25:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T12:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      quickFriday1 ++ quickFriday2
    }

    // CONFERENCE KEYNOTES
    val keynoteSlotsThursday: List[Slot] = {
     val keynoteThursdayWelcome = ConferenceRooms.keynoteRoom.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "thursday",
            new DateTime("2016-04-21T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T09:15:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val keynoteThursdaySlot1 = ConferenceRooms.keynoteRoom.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "thursday",
            new DateTime("2016-04-21T09:20:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T09:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val keynoteThursdaySlot2 = ConferenceRooms.keynoteRoom.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "thursday",
            new DateTime("2016-04-21T09:45:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T10:05:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      val keynoteThursdaySlot3 = ConferenceRooms.keynoteRoom.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "thursday",
            new DateTime("2016-04-21T10:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T10:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r3)
      }

      keynoteThursdayWelcome ++keynoteThursdaySlot1 ++ keynoteThursdaySlot2 ++ keynoteThursdaySlot3
    }

    val keynoteSlotsFriday: List[Slot] = {
      val keynoteFridaySlot1 = ConferenceRooms.keynoteRoom.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "friday",
            new DateTime("2016-04-22T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T09:20:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val keynoteFridaySlot2 = ConferenceRooms.keynoteRoom.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "friday",
            new DateTime("2016-04-22T09:25:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T09:45:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      val keynoteFridaySlot3 = ConferenceRooms.keynoteRoom.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "friday",
            new DateTime("2016-04-22T09:50:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T10:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r3)
      }
      val keynoteFridaySlot4 = ConferenceRooms.keynoteRoom.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "friday",
            new DateTime("2016-04-22T10:15:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T10:35:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r4)
      }

      keynoteFridaySlot1 ++ keynoteFridaySlot2 ++ keynoteFridaySlot3 ++ keynoteFridaySlot4

    }

    // CONFERENCE SLOTS
    val conferenceSlotsThursday: List[Slot] = {

      val conferenceThursdaySlot1 = ConferenceRooms.allRoomsConf.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "thursday",
            new DateTime("2016-04-21T11:15:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T12:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val conferenceThursdaySlot2 = ConferenceRooms.allRoomsConf.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "thursday",
            new DateTime("2016-04-21T12:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      val conferenceThursdaySlot3 = ConferenceRooms.allRoomsConf.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "thursday",
            new DateTime("2016-04-21T13:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T14:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r3)
      }
      val conferenceThursdaySlot4 = ConferenceRooms.allRoomsConf.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "thursday",
            new DateTime("2016-04-21T14:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T15:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r4)
      }
      val conferenceThursdaySlot5 = ConferenceRooms.allRoomsConf.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "thursday",
            new DateTime("2016-04-21T16:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T16:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r5)
      }
      val conferenceThursdaySlot6 = ConferenceRooms.allRoomsConf.map {
        r6 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "thursday",
            new DateTime("2016-04-21T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-21T17:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r6)
      }
      conferenceThursdaySlot1 ++ conferenceThursdaySlot2 ++ conferenceThursdaySlot3 ++ conferenceThursdaySlot4 ++ conferenceThursdaySlot5 ++ conferenceThursdaySlot6
    }

    val conferenceSlotsFriday: List[Slot] = {

      val conferenceFridaySlot1 = ConferenceRooms.allRoomsConf.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T11:15:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T12:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r1)
      }
      val conferenceFridaySlot2 = ConferenceRooms.allRoomsConf.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T12:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r2)
      }
      val conferenceFridaySlot3 = ConferenceRooms.allRoomsConf.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T13:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T14:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r3)
      }
      val conferenceFridaySlot4 = ConferenceRooms.allRoomsConf.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T14:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T15:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r4)
      }
      val conferenceFridaySlot5 = ConferenceRooms.allRoomsConf.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T16:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T16:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r5)
      }
      val conferenceFridaySlot5extra = ConferenceRooms.allRoomsConf.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T17:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), r5)
      }

      // Cast codeur
      val conferenceFridaySlot6 = List(ConferenceRooms.NEUILLY_252AB).map {
        rcc =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "friday",
            new DateTime("2016-04-22T18:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
            new DateTime("2016-04-22T18:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")), rcc)
      }

      conferenceFridaySlot1 ++ conferenceFridaySlot2 ++ conferenceFridaySlot3 ++ conferenceFridaySlot4 ++ conferenceFridaySlot5 ++ conferenceFridaySlot5extra ++ conferenceFridaySlot6
    }

    // Registration, coffee break, lunch etc
    val wednesdayBreaks = List(
      SlotBuilder(ConferenceSlotBreaks.registration, "wednesday",
        new DateTime("2016-04-20T08:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-20T09:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
      , SlotBuilder(ConferenceSlotBreaks.lunch, "wednesday",
        new DateTime("2016-04-20T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-20T13:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
      , SlotBuilder(ConferenceSlotBreaks.coffee, "wednesday",
        new DateTime("2016-04-20T16:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-20T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
    )

    val thursdayBreaks = List(
      SlotBuilder(ConferenceSlotBreaks.registration, "thursday",
        new DateTime("2016-04-21T07:30:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-21T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "thursday",
        new DateTime("2016-04-21T10:45:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-21T11:15:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
      , SlotBuilder(ConferenceSlotBreaks.lunch, "thursday",
        new DateTime("2016-04-21T12:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-21T12:55:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
      , SlotBuilder(ConferenceSlotBreaks.coffee, "thursday",
        new DateTime("2016-04-21T15:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-21T16:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")))
    )

    val fridayBreaks = List(
      SlotBuilder(ConferenceSlotBreaks.petitDej, "friday",
        new DateTime("2016-04-22T08:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-22T09:00:00.000+02:00"))
      , SlotBuilder(ConferenceSlotBreaks.coffee, "friday",
        new DateTime("2016-04-22T10:45:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-22T11:15:00.000+02:00"))
      , SlotBuilder(ConferenceSlotBreaks.lunch, "friday",
        new DateTime("2016-04-22T12:00:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-22T12:55:00.000+02:00"))
      , SlotBuilder(ConferenceSlotBreaks.coffee, "friday",
        new DateTime("2016-04-22T15:40:00.000+02:00").toDateTime(DateTimeZone.forID("America/Sao_Paulo")),
        new DateTime("2016-04-22T16:10:00.000+02:00"))
    )

    val wednesday: List[Slot] = {
      wednesdayBreaks ++ universitySlotsWednesday ++ tiaSlotsWednesday ++ labsSlotsWednesday
    }

    val thursday: List[Slot] = {
      thursdayBreaks ++ keynoteSlotsThursday ++ conferenceSlotsThursday ++ quickiesSlotsThursday ++ bofSlotsThursday ++ labsSlotsThursday ++ tiaSlotsThursday
    }

    val friday: List[Slot] = {
      fridayBreaks ++ keynoteSlotsFriday ++ conferenceSlotsFriday ++ quickiesSlotsFriday ++ labsSlotsFriday
    }

    // COMPLETE DEVOXX
    def all: List[Slot] = {
      wednesday ++ thursday ++ friday
    }
  }

  def dateRange(from: DateTime, to: DateTime, step: Period): Iterator[DateTime]      =Iterator.iterate(from)(_.plus(step)).takeWhile(!_.isAfter(to))

  val fromDay = new DateTime().withYear(2018).withMonthOfYear(12).withDayOfMonth(5)
  val toDay = new DateTime().withYear(2018).withMonthOfYear(12).withDayOfMonth(8)

  // TODO You might want to start here and configure first, your various Conference Elements
  def current() = ConferenceDescriptor(
    eventCode = "TDC2018POA",
    // You will need to update conf/routes files with this code if modified
    confUrlCode = "tdc2018poa",
    frLangEnabled = false,
    fromEmail = Play.current.configuration.getString("mail.from").getOrElse("organizacao@thedevelopersconference.com.br"),
    committeeEmail = Play.current.configuration.getString("mail.committee.email").getOrElse("organizacao@thedevelopersconference.com.br"),
    bccEmail = Play.current.configuration.getString("mail.bcc"),
    bugReportRecipient = Play.current.configuration.getString("mail.bugreport.recipient").getOrElse("tdc@thedevelopersconference.com.br"),
    conferenceUrls = ConferenceUrls(
      faq = "http://cfp-poa.thedevconf.com.br/faq",
      registration = "http://thedevconf.com.br/tdc/2018/inscricoes",
      confWebsite = "http:/thedevconf.com.br",
      cfpHostname = Play.current.configuration.getString("cfp.hostname").getOrElse("cfp-poa.thedevconf.com.br")
    ),
    timing = ConferenceTiming(
      datesI18nKey = "5 a 8 de Dezembro de 2018",
      speakersPassDuration = 4,
      preferredDayEnabled = true,
      firstDayFr = "5 december",
      firstDayEn = "december 5th",
      datesFr = "du 5 au 8 december 2018",
      datesEn = "December 5th to 8th, 2018",
      cfpOpenedOn = DateTime.parse("2018-09-05T19:30:00-03:00"),
      cfpClosedOn = DateTime.parse("2018-09-26T23:59:59-03:00"),
      scheduleAnnouncedOn = DateTime.parse("2018-10-02T00:00:00-03:00"),
      days=dateRange(fromDay,toDay,new Period().withDays(1))
    ),
    hosterName = "AWS", hosterWebsite = "http://aws.amazon.com/",
    hashTag = "#TheDevConf",
    conferenceSponsor = ConferenceSponsor(showSponsorProposalCheckbox = true, sponsorProposalType = ConferenceProposalTypes.CONF)
    ,  List(new Locale("pt","BR"))
    , "UniRitter, Porto Alegre, RS"
    , notifyProposalSubmitted = false // Do not send an email for each talk submitted for France
    , 700 // 1200 // French developers tends to be a bit verbose... we need extra space :-)
  )

  // It has to be a def, not a val, else it is not re-evaluated
  def isCFPOpen: Boolean = {
    Play.current.configuration.getBoolean("cfp.isOpen").getOrElse(false)
  }

  def isGoldenTicketActive:Boolean = Play.current.configuration.getBoolean("goldenTicket.active").getOrElse(false)

  def isFavoritesSystemActive:Boolean = Play.current.configuration.getBoolean("cfp.activateFavorites").getOrElse(false)

  def isHTTPSEnabled = Play.current.configuration.getBoolean("cfp.activateHTTPS").getOrElse(false)

}


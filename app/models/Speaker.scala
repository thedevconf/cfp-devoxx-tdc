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

package models

import com.github.rjeschke.txtmark.Processor
import library.{Benchmark, Redis, ZapJson}
import org.apache.commons.lang3.StringUtils
import org.joda.time.{DateTime, Instant}
import play.api.libs.json._
import play.twirl.api.HtmlFormat

/**
 * Speaker profile, is used mainly to show details.
 *
 * Webuser is the "technical" and internal web user representation.
 *
 * Author: nicolas martignole
 * Created: 28/09/2013 11:01
 */
case class Location(country: Option[String], state: Option[String], city: Option[String])
object Location {
  implicit val LocationFormat = Json.format[Location]

  val countries: Seq[(String, String)] = {
    List(
      ("AF" , "Afghanistan"),
      ("AL" , "Albania"),
      ("DZ" , "Algeria"),
      ("AS" , "American Samoa"),
      ("AD" , "Andorra"),
      ("AO" , "Angola"),
      ("AI" , "Anguilla"),
      ("AQ" , "Antarctica"),
      ("AG" , "Antigua And Barbuda"),
      ("AR" , "Argentina"),
      ("AM" , "Armenia"),
      ("AW" , "Aruba"),
      ("AU" , "Australia"),
      ("AT" , "Austria"),
      ("AZ" , "Azerbaijan"),
      ("BS" , "Bahamas"),
      ("BH" , "Bahrain"),
      ("BD" , "Bangladesh"),
      ("BB" , "Barbados"),
      ("BY" , "Belarus"),
      ("BE" , "Belgium"),
      ("BZ" , "Belize"),
      ("BJ" , "Benin"),
      ("BM" , "Bermuda"),
      ("BT" , "Bhutan"),
      ("BO" , "Bolivia"),
      ("BA" , "Bosnia And Herzegovina"),
      ("BW" , "Botswana"),
      ("BV" , "Bouvet Island"),
      ("BR" , "Brazil"),
      ("IO" , "British Indian Ocean Territory"),
      ("BN" , "Brunei Darussalam"),
      ("BG" , "Bulgaria"),
      ("BF" , "Burkina Faso"),
      ("BI" , "Burundi"),
      ("KH" , "Cambodia"),
      ("CM" , "Cameroon"),
      ("CA" , "Canada"),
      ("CV" , "Cape Verde"),
      ("KY" , "Cayman Islands"),
      ("CF" , "Central African Republic"),
      ("TD" , "Chad"),
      ("CL" , "Chile"),
      ("CN" , "China"),
      ("CX" , "Christmas Island"),
      ("CC" , "Cocos (keeling) Islands"),
      ("CO" , "Colombia"),
      ("KM" , "Comoros"),
      ("CG" , "Congo"),
      ("CD" , "Congo, The Democratic Republic Of The"),
      ("CK" , "Cook Islands"),
      ("CR" , "Costa Rica"),
      ("CI" , "Cote D'ivoire"),
      ("HR" , "Croatia"),
      ("CU" , "Cuba"),
      ("CY" , "Cyprus"),
      ("CZ" , "Czech Republic"),
      ("DK" , "Denmark"),
      ("DJ" , "Djibouti"),
      ("DM" , "Dominica"),
      ("DO" , "Dominican Republic"),
      ("TP" , "East Timor"),
      ("EC" , "Ecuador"),
      ("EG" , "Egypt"),
      ("SV" , "El Salvador"),
      ("GQ" , "Equatorial Guinea"),
      ("ER" , "Eritrea"),
      ("EE" , "Estonia"),
      ("ET" , "Ethiopia"),
      ("FK" , "Falkland Islands (malvinas)"),
      ("FO" , "Faroe Islands"),
      ("FJ" , "Fiji"),
      ("FI" , "Finland"),
      ("FR" , "France"),
      ("GF" , "French Guiana"),
      ("PF" , "French Polynesia"),
      ("TF" , "French Southern Territories"),
      ("GA" , "Gabon"),
      ("GM" , "Gambia"),
      ("GE" , "Georgia"),
      ("DE" , "Germany"),
      ("GH" , "Ghana"),
      ("GI" , "Gibraltar"),
      ("GR" , "Greece"),
      ("GL" , "Greenland"),
      ("GD" , "Grenada"),
      ("GP" , "Guadeloupe"),
      ("GU" , "Guam"),
      ("GT" , "Guatemala"),
      ("GN" , "Guinea"),
      ("GW" , "Guinea-bissau"),
      ("GY" , "Guyana"),
      ("HT" , "Haiti"),
      ("HM" , "Heard Island And Mcdonald Islands"),
      ("VA" , "Holy See (vatican City State)"),
      ("HN" , "Honduras"),
      ("HK" , "Hong Kong"),
      ("HU" , "Hungary"),
      ("IS" , "Iceland"),
      ("IN" , "India"),
      ("ID" , "Indonesia"),
      ("IR" , "Iran, Islamic Republic Of"),
      ("IQ" , "Iraq"),
      ("IE" , "Ireland"),
      ("IL" , "Israel"),
      ("IT" , "Italy"),
      ("JM" , "Jamaica"),
      ("JP" , "Japan"),
      ("JO" , "Jordan"),
      ("KZ" , "Kazakstan"),
      ("KE" , "Kenya"),
      ("KI" , "Kiribati"),
      ("KP" , "Korea, Democratic People's Republic Of"),
      ("KR" , "Korea, Republic Of"),
      ("KV" , "Kosovo"),
      ("KW" , "Kuwait"),
      ("KG" , "Kyrgyzstan"),
      ("LA" , "Lao People's Democratic Republic"),
      ("LV" , "Latvia"),
      ("LB" , "Lebanon"),
      ("LS" , "Lesotho"),
      ("LR" , "Liberia"),
      ("LY" , "Libyan Arab Jamahiriya"),
      ("LI" , "Liechtenstein"),
      ("LT" , "Lithuania"),
      ("LU" , "Luxembourg"),
      ("MO" , "Macau"),
      ("MK" , "Macedonia, The Former Yugoslav Republic Of"),
      ("MG" , "Madagascar"),
      ("MW" , "Malawi"),
      ("MY" , "Malaysia"),
      ("MV" , "Maldives"),
      ("ML" , "Mali"),
      ("MT" , "Malta"),
      ("MH" , "Marshall Islands"),
      ("MQ" , "Martinique"),
      ("MR" , "Mauritania"),
      ("MU" , "Mauritius"),
      ("YT" , "Mayotte"),
      ("MX" , "Mexico"),
      ("FM" , "Micronesia, Federated States Of"),
      ("MD" , "Moldova, Republic Of"),
      ("MC" , "Monaco"),
      ("MN" , "Mongolia"),
      ("MS" , "Montserrat"),
      ("ME" , "Montenegro"),
      ("MA" , "Morocco"),
      ("MZ" , "Mozambique"),
      ("MM" , "Myanmar"),
      ("NA" , "Namibia"),
      ("NR" , "Nauru"),
      ("NP" , "Nepal"),
      ("NL" , "Netherlands"),
      ("AN" , "Netherlands Antilles"),
      ("NC" , "New Caledonia"),
      ("NZ" , "New Zealand"),
      ("NI" , "Nicaragua"),
      ("NE" , "Niger"),
      ("NG" , "Nigeria"),
      ("NU" , "Niue"),
      ("NF" , "Norfolk Island"),
      ("MP" , "Northern Mariana Islands"),
      ("NO" , "Norway"),
      ("OM" , "Oman"),
      ("PK" , "Pakistan"),
      ("PW" , "Palau"),
      ("PS" , "Palestinian Territory, Occupied"),
      ("PA" , "Panama"),
      ("PG" , "Papua New Guinea"),
      ("PY" , "Paraguay"),
      ("PE" , "Peru"),
      ("PH" , "Philippines"),
      ("PN" , "Pitcairn"),
      ("PL" , "Poland"),
      ("PT" , "Portugal"),
      ("PR" , "Puerto Rico"),
      ("QA" , "Qatar"),
      ("RE" , "Reunion"),
      ("RO" , "Romania"),
      ("RU" , "Russian Federation"),
      ("RW" , "Rwanda"),
      ("SH" , "Saint Helena"),
      ("KN" , "Saint Kitts And Nevis"),
      ("LC" , "Saint Lucia"),
      ("PM" , "Saint Pierre And Miquelon"),
      ("VC" , "Saint Vincent And The Grenadines"),
      ("WS" , "Samoa"),
      ("SM" , "San Marino"),
      ("ST" , "Sao Tome And Principe"),
      ("SA" , "Saudi Arabia"),
      ("SN" , "Senegal"),
      ("RS" , "Serbia"),
      ("SC" , "Seychelles"),
      ("SL" , "Sierra Leone"),
      ("SG" , "Singapore"),
      ("SK" , "Slovakia"),
      ("SI" , "Slovenia"),
      ("SB" , "Solomon Islands"),
      ("SO" , "Somalia"),
      ("ZA" , "South Africa"),
      ("GS" , "South Georgia And The South Sandwich Islands"),
      ("ES" , "Spain"),
      ("LK" , "Sri Lanka"),
      ("SD" , "Sudan"),
      ("SR" , "Suriname"),
      ("SJ" , "Svalbard And Jan Mayen"),
      ("SZ" , "Swaziland"),
      ("SE" , "Sweden"),
      ("CH" , "Switzerland"),
      ("SY" , "Syrian Arab Republic"),
      ("TW" , "Taiwan, Province Of China"),
      ("TJ" , "Tajikistan"),
      ("TZ" , "Tanzania, United Republic Of"),
      ("TH" , "Thailand"),
      ("TG" , "Togo"),
      ("TK" , "Tokelau"),
      ("TO" , "Tonga"),
      ("TT" , "Trinidad And Tobago"),
      ("TN" , "Tunisia"),
      ("TR" , "Turkey"),
      ("TM" , "Turkmenistan"),
      ("TC" , "Turks And Caicos Islands"),
      ("TV" , "Tuvalu"),
      ("UG" , "Uganda"),
      ("UA" , "Ukraine"),
      ("AE" , "United Arab Emirates"),
      ("GB" , "United Kingdom"),
      ("US" , "United States"),
      ("UM" , "United States Minor Outlying Islands"),
      ("UY" , "Uruguay"),
      ("UZ" , "Uzbekistan"),
      ("VU" , "Vanuatu"),
      ("VE" , "Venezuela"),
      ("VN" , "Viet Nam"),
      ("VG" , "Virgin Islands, British"),
      ("VI" , "Virgin Islands, U.s."),
      ("WF" , "Wallis And Futuna"),
      ("EH" , "Western Sahara"),
      ("YE" , "Yemen"),
      ("ZM" , "Zambia"),
      ("ZW" , "Zimbabwe")
    )
    }.toSeq
}

case class Speaker(uuid: String
                   , email: String
                   , name: Option[String]
                   , bio: String
                   , lang: Option[String]
                   , twitter: Option[String]
                   , avatarUrl: Option[String]
                   , company: Option[String]
                   , blog: Option[String]
                   , firstName: Option[String]
                   , qualifications: Option[String]
                   , phone: Option[String]
                   , cpf: Option [String]
                   , city: Option [String]
                   , state: Option[String]
                   , country: Option[String]
                   , gender: Option[String]
                   , tshirtSize: Option[String]
                   , linkedIn: Option[String]
                   , github: Option[String]
                   , tagName: Option[String]
                   , facebook: Option[String]
                   , instagram: Option[String]
                   , race: Option[String]
                   , disability: Option[String]
                  ) {

  def cleanName: String = {
    firstName.getOrElse("").capitalize + name.map(n => " " + n).getOrElse("").capitalize
  }

  def cleanShortName: String = {
    firstName.map(_.charAt(0)).getOrElse("") + name.map(n => "." + n).getOrElse("")
  }

  def urlName: String = {
    StringUtils.stripAccents(cleanName).replaceAll(" ", "_").toLowerCase
  }

  def cleanLang: String = lang.map {
    l =>
      val cleanL = if (l.contains(",")) {
        l.substring(0, l.indexOf(","))
      } else {
        l.toLowerCase
      }
      if (cleanL.contains("-")) {
        cleanL.substring(0, l.indexOf("-"))
      } else {
        cleanL
      }
  }.getOrElse("pt")

  def cleanTwitter: Option[String] = twitter.map {
    tw =>
      val trimmed = tw.trim()
      if (!trimmed.startsWith("@")) {
        "@" + trimmed
      } else {
        trimmed
      }
  }


  def hasTwitter = StringUtils.trimToEmpty(twitter.getOrElse("")).nonEmpty

  def hasLinkedIn = StringUtils.trimToEmpty(linkedIn.getOrElse("")).nonEmpty

  def hasGithub = StringUtils.trimToEmpty(github.getOrElse("")).nonEmpty

  def hasFacebook = StringUtils.trimToEmpty(facebook.getOrElse("")).nonEmpty

  def hasInstagram = StringUtils.trimToEmpty(instagram.getOrElse("")).nonEmpty

  def hasBio = StringUtils.trimToEmpty(bio).nonEmpty

  def hasCompany = StringUtils.trimToEmpty(company.getOrElse("")).nonEmpty

  def hasAvatar = StringUtils.trimToEmpty(avatarUrl.getOrElse("")).nonEmpty

  def hasBlog = StringUtils.trimToEmpty(blog.getOrElse("")).nonEmpty

  lazy val bioAsHtml: String = {
    val escapedHtml = HtmlFormat.escape(bio).body // escape HTML code and JS
    val processedMarkdownTest = Processor.process(StringUtils.trimToEmpty(escapedHtml).trim()) // Then do markdown processing
    processedMarkdownTest
  }

  def hasPhone = StringUtils.trimToEmpty(phone.getOrElse("")).nonEmpty

  def hasGender = StringUtils.trimToEmpty(gender.getOrElse("")).nonEmpty

  def hasRace = StringUtils.trimToEmpty(race.getOrElse("")).nonEmpty

  def hasDisability = StringUtils.trimToEmpty(disability.getOrElse("")).nonEmpty

  def hasTshirtSize = StringUtils.trimToEmpty(tshirtSize.getOrElse("")).nonEmpty

}

object Speaker {

  def conferenceId = ConferenceDescriptor.current().eventCode

  implicit val speakerFormat = Json.format[Speaker]

  val countries = Location.countries

  val genders = Seq("M" -> "Masculino", "F" -> "Feminino" , "N" -> "Outro", "P" -> "Prefiro não responder")

  val races = Seq("branco" -> "Branco(a)", "indigena" -> "Indígena", "mestico" -> "Mestiço(a)", "negro" -> "Negro(a)", "oriental" -> "Oriental", "nao sei" -> "Não Sei", "prefiro nao responder" -> "Prefiro não responder")

  val disabilities = Seq("nao" -> "Não"
    ,"deficiencia visual cego" -> "Sim - deficiência visual, cego"
    ,"deficiencia visual baixa visao" -> "Sim - deficiência visual, baixa visão"
    ,"deficiencia auditiva parcial" -> "Sim - deficiência auditiva parcial"
    ,"deficiencia auditiva total" -> "Sim - deficiência auditiva total"
    ,"restricao de mobilidade parcial" -> "Sim - restrição de mobilidade parcial"
    ,"restricao de mobilidade cadeirante" -> "Sim - restrição de mobilidade cadeirante")

  val sizes = Seq(("P", "P"), ("M","M"), ("G","G"), ("GG","GG"), ("XGG","XGG"), ("XXGG","XXGG"))

  def createSpeaker(webuserUUID:String, email: String, name: String,
                    bio: String, lang: Option[String], avatarUrl: Option[String],
                    company: Option[String], blog: Option[String], firstName: String,
                    qualifications: String, phone: String, cpf: Option[String], location: Location,
                    gender: Option[String], tshirtSize: Option[String], tagname: String,
                    race: Option[String], disability: Option[String], socialMedia: SocialMedia): Speaker = {
    Speaker(webuserUUID, email.trim().toLowerCase, Option(name), bio, lang, socialMedia.twitter, avatarUrl, company,
      blog, Some(firstName), Option(qualifications), Option(phone), Option(cpf), location.city, location.state,
      location.country, gender, tshirtSize, socialMedia.linkedIn, socialMedia.github, Option(tagname),
      socialMedia.facebook, socialMedia.instagram, race, disability)
  }

  def createOrEditSpeaker(uuid: Option[String], email: String, name: String, bio: String, lang: Option[String], avatarUrl: Option[String],
                          company: Option[String], blog: Option[String], firstName: String, acceptTerms: Boolean,
                          qualifications: String, phone: Option[String], cpf: Option[String], location: Location,
                          gender: Option[String], tshirtSize: Option[String], tagName: String, race: Option[String],
                          disability: Option[String], socialMedia: SocialMedia): Speaker = {
    uuid match {
      case None =>
        val newUUID = Webuser.generateUUID(email)
        if (acceptTerms) {
          doAcceptTerms(newUUID)
        } else {
          refuseTerms(newUUID)
        }
        Speaker(newUUID, email.trim().toLowerCase, Option(name), bio, lang, socialMedia.twitter, avatarUrl
          , company, blog, Option(firstName), Option(qualifications), phone, cpf, location.city, location.state
          , location.country, gender, tshirtSize, socialMedia.linkedIn, socialMedia.github, Option(tagName)
          , socialMedia.facebook, socialMedia.instagram, race, disability)
      case Some(validUuid) =>
        if (acceptTerms) {
          doAcceptTerms(validUuid)
        } else {
          refuseTerms(validUuid)
        }
        Speaker(validUuid, email.trim().toLowerCase, Option(name), bio, lang, socialMedia.twitter, avatarUrl
          , company, blog, Option(firstName), Option(qualifications), phone, cpf, location.city, location.state
          , location.country, gender, tshirtSize, socialMedia.linkedIn, socialMedia.github, Option(tagName)
          , socialMedia.facebook, socialMedia.instagram, race, disability)
    }

  }

  def unapplyForm(s: Speaker): Option[(String, String, String, String, Option[String], Option[String], Option[String], Option[String], String, String, String, Option[String], Option[String], String, Option[String], Option[String], SocialMedia)] = {
    Some(("xxx",s.email, s.name.getOrElse(""), s.bio, s.lang, s.avatarUrl, s.company, s.blog, s.firstName.getOrElse(""), s.qualifications.getOrElse("No experience"),
      s.phone.getOrElse(""), s.cpf.getOrElse(""), Location(s.city, s.state, s.country), s.gender, s.tshirtSize, s.tagName.getOrElse(""), s.race, s.disability,
      SocialMedia(s.twitter, s.linkedIn, s.github, s.facebook,s.instagram)))
  }

  def unapplyFormEdit(s: Speaker): Option[(Option[String], String, String, String, Option[String], Option[String], Option[String], Option[String], String, Boolean, String, Option[String], Option[String], Option[String], String, Option[String],Option[String], SocialMedia)] = {
    Some((Option(s.uuid), s.email, s.name.getOrElse(""), s.bio, s.lang, s.avatarUrl, s.company, s.blog, s.firstName.getOrElse(""), needsToAccept(s.uuid) == false, s.qualifications.getOrElse("No experience"),
      s.phone, s.cpf, Location(s.city, s.state, s.country), s.gender, s.tshirtSize, s.tagName.getOrElse(""), s.race, s.disability,
      SocialMedia(s.twitter, s.linkedIn, s.github, s.facebook,s.instagram)))
  }

  def save(speaker: Speaker) = Redis.pool.withClient {
    client =>
      val jsonSpeaker = Json.stringify(Json.toJson(speaker))
      client.hset("Speaker", speaker.uuid, jsonSpeaker)
  }

  def update(uuid: String, speaker: Speaker) = Redis.pool.withClient {
    client =>
      val jsonSpeaker = Json.stringify(Json.toJson(speaker.copy(uuid = uuid)))
      client.hset("Speaker", uuid, jsonSpeaker)
  }

  def updateName(uuid: String, firstName: String, lastName: String) = {
    findByUUID(uuid).map {
      speaker =>
        Speaker.update(uuid, speaker.copy(name = Option(StringUtils.trimToNull(lastName)), firstName = Option(StringUtils.trimToNull(firstName))))
    }
  }

  def findByUUID(uuid: String): Option[Speaker] = Redis.pool.withClient {
    client =>
      client.hget("Speaker", uuid).flatMap {
        json: String =>
          Json.parse(json).validate[Speaker].fold(invalid => {
            play.Logger.error("Invalid json format for Speaker, unable to unmarshall " + ZapJson.showError(invalid))
            None
          }, validSpeaker => Some(validSpeaker))
      }
  }

  def delete(uuid: String) = Redis.pool.withClient {
    client =>
      client.hdel("Speaker", uuid)
  }

  def allSpeakers(): List[Speaker] = Redis.pool.withClient {
    client =>
      client.hvals("Speaker").flatMap {
        jsString =>
          val maybeSpeaker = Json.parse(jsString).asOpt[Speaker]
          maybeSpeaker
      }
  }

  def withOneProposal(speakers: List[Speaker]) = {
    speakers.filter(s => Proposal.hasOneAcceptedProposal(s.uuid))
  }

  def notMemberOfCFP(speakers: List[Speaker]) = {
    speakers.filterNot(s => Webuser.isMember(s.uuid, "cfp"))
  }

  def allSpeakersUUID(): Set[String] = Redis.pool.withClient {
    client =>
      client.hkeys("Speaker")
  }

  def loadSpeakersFromSpeakerIDs(speakerIDs: Set[String]): List[Speaker] = Redis.pool.withClient {
    client =>
      client.hmget("Speaker", speakerIDs).flatMap {
        js: String =>
          Json.parse(js).asOpt[Speaker]
      }
  }

  def countAll(): Long = Redis.pool.withClient {
    client =>
      client.hlen("Speaker")
  }

  def needsToAccept(speakerId: String) = Redis.pool.withClient {
    client =>
      !client.hexists("TermsAndConditions", speakerId)
  }

  def doAcceptTerms(speakerId: String) = Redis.pool.withClient {
    client =>
      client.hset("TermsAndConditions", speakerId, new Instant().getMillis.toString)
  }

  def refuseTerms(speakerId: String) = Redis.pool.withClient {
    client =>
      client.hdel("TermsAndConditions", speakerId)
  }

  def getAcceptedDate(speakerId: String): Option[DateTime] = Redis.pool.withClient {
    client =>
      client.hget("TermsAndConditions", speakerId).map {
        dateStr: String =>
          new org.joda.time.Instant(dateStr).toDateTime
      }
  }

  def allSpeakersWithAcceptedTerms() = Redis.pool.withClient {
    client =>

      val termKeys = Benchmark.measure(()=>
        client.hkeys("TermsAndConditions")
        ,"termKeys")

      val speakerIDs = Benchmark.measure(() =>
        termKeys.filter(uuid => Proposal.hasOneAcceptedProposal(uuid))
        ,"speakerIDs")

      val allSpeakers = Benchmark.measure(() =>
        client.hmget("Speaker", speakerIDs).flatMap {
          json: String =>
            Json.parse(json).validate[Speaker].fold(invalid => {
              play.Logger.error("Speaker error. " + ZapJson.showError(invalid))
              None
            }, validSpeaker => Some(validSpeaker))
        },"allSpeakers")
      allSpeakers
  }

  def allThatDidNotAcceptedTerms(): Set[String] = Redis.pool.withClient {
    client =>
      val allSpeakerIDs = client.keys(s"ApprovedSpeakers:$conferenceId:*").map(s => s.substring(s"ApprovedSpeakers:$conferenceId:".length))
      val allThatAcceptedConditions = client.hkeys("TermsAndConditions")
      allSpeakerIDs.diff(allThatAcceptedConditions)
  }


}
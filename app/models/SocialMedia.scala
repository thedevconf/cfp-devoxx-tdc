package models

import play.api.data.validation.{Constraint,Valid,Invalid,ValidationError}
import play.api.i18n.Messages

case class SocialMedia(twitter:Option[String],linkedIn:Option[String],github:Option[String],facebook:Option[String],instagram:Option[String])

object SocialMedia {

    val twitterPrefix = "https://twitter\\.com/.+".r
    val linkedInPrefix = "https://www\\.linkedin\\.com/in/.+".r
    val githubPrefix = "https://github\\.com/.+".r
    val facebookPrefix = "https://www\\.facebook\\.com/.+".r
    val instagramPrefix = "https://www\\.instagram\\.com/.+".r

    val twitterURL:Constraint[String] = Constraint("constraint.twitter"){plainText =>
      plainText match {
        case twitterPrefix() => Valid
        case _ => Invalid(Seq(ValidationError(Messages("constraint.twitter.error"))))
      }
    }

    val linkedInURL:Constraint[String] = Constraint("constraint.linkedin"){plainText =>
      plainText match {
        case linkedInPrefix() => Valid
        case _ => Invalid(Seq(ValidationError(Messages("constraint.linkedin.error"))))
      }
    }

    val githubURL:Constraint[String] = Constraint("constraint.github"){plainText =>
      plainText match {
        case githubPrefix() => Valid
        case _ => Invalid(Seq(ValidationError(Messages("constraint.github.error"))))
      }
    }

    val facebookURL:Constraint[String] = Constraint("constraint.facebook"){plainText =>
      plainText match {
        case facebookPrefix() => Valid
        case _ => Invalid(Seq(ValidationError(Messages("constraint.facebook.error"))))
      }
    }

    val instagramURL:Constraint[String] = Constraint("constraint.instagram"){plainText =>
      plainText match {
        case instagramPrefix() => Valid
        case _ => Invalid(Seq(ValidationError(Messages("constraint.instagram.error"))))
      }
    }

}
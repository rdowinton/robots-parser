package org.rdowinton.robots

import scala.util.parsing.combinator._

abstract class GroupMember(value: String)
case class Allow(value: String) extends GroupMember(value)
case class Disallow(value: String) extends GroupMember(value)
case class NonGroupField(key: String, value: String) {
  override def toString: String = key + ": " + value
}
case class RobotsFile(entries: List[RobotsEntry], nonGroupFields: List[NonGroupField]) {
  override def toString: String = entries.mkString("\r\n\r\n") + "\r\n\r\n" + nonGroupFields.mkString("\r\n")

  def canVisit(userAgent: String, path: String): Boolean = {
    /**
      * Replaces wildcard character with regex equivalent and matches against a provided string
      * TODO: Improve this, there may be other characters besides '?' and '.' that need escaping
      */
    def compare(value: String)(against: String): Boolean =
      ("^" + (if(value.last == '/') value + '*' else value)) // Replace "/$" with "/*$" e.g. "/test/" = "/test/*"
        .replaceAll("\\?", "\\\\?")
        .replaceAll("\\.", "\\\\.")
        .replaceAll("\\*", ".*").r.findFirstMatchIn(against).isDefined

    val applicable = entries.filter(
      e => e.userAgents.count(ua => compare(ua)(userAgent)) > 0
    )
    val allows = applicable.flatMap(a => a.allows).filter(a => {
      compare(a.value)(path)
    })
    val disallows = applicable.flatMap(a => a.disallows).filter(d => compare(d.value)(path))

    val longestAllow = allows.map(_.value).reduceLeftOption((a, b) => if(a > b) a else b).getOrElse("")
    val longestDisallow = disallows.map(_.value).reduceLeftOption((a, b) => if(a > b) a else b).getOrElse("")

    longestAllow.length >= longestDisallow.length
  }
}
case class RobotsEntry(userAgents: List[String], groupMembers: List[GroupMember]) {
  override def toString: String = {
    val uas = userAgents.map("User-agent: " + _).mkString("\r\n")
    val gms = groupMembers.map({
      case Allow(value) => "Allow: " + value
      case Disallow(value) => "Disallow: " + value
    }).mkString("\r\n")

    List(uas, gms).mkString("\r\n")
  }

  def allows: List[Allow] = groupMembers.filter(_.isInstanceOf[Allow]).map(_.asInstanceOf[Allow])
  def disallows: List[Disallow] = groupMembers.filter(_.isInstanceOf[Disallow]).map(_.asInstanceOf[Disallow])
}
object Robots extends RegexParsers {

  override def skipWhitespace = false

  val eol = """[\r?\n]+""".r
  val lws = """[\s]+""".r
  val comment = opt(lws) ~ "#" ~> opt("""[^\r\n]+""".r)

  /* eol is optional in case there is no eol after last entry */
  val userAgent = """(?i)User-agent:[\s]*""".r ~> """([^#\r\n]+)""".r <~ opt(comment) ~ opt(eol)
  val allow = """(?i)Allow:[\s]*""".r ~> """([^#\s]+)""".r <~ opt(comment) ~ opt(eol) ^^ {
    case value => Allow(value)
  }
  val disallow = """(?i)Disallow:[\s]*""".r ~> """([^#\s]+)""".r <~ opt(comment) ~ opt(eol) ^^ {
    case value => Disallow(value)
  }

  /* For Sitemap, Host etc. */
  def nonGroupField = """^(?i)(?!User\-agent|Allow|Disallow)[a-zA-Z0-9-]+""".r ~
    """:[\s]*""".r ~ """([^#\r\n]+)""".r <~ opt(comment) ~ opt(eol) ^^ {
    case key ~ delimiter ~ value => NonGroupField(key, value)
  }

  /* rep1 for user-agent as at least one is required for a group */
  def userAgents: Parser[List[String]] = rep1(userAgent)

  /* rep for allow/disallow and non-group fields as they are optional */
  def groupMembers: Parser[List[GroupMember]] = rep(allow | disallow)
  def nonGroupFields: Parser[List[NonGroupField]] = rep(nonGroupField)

  def fields: Parser[List[Either[NonGroupField, GroupMember]]] =
    rep(allow ^^ { case r => Right(r) } | disallow ^^ { case r => Right(r) } | nonGroupField ^^ { case r => Left(r) })

  def entry:Parser[(RobotsEntry, List[NonGroupField])] =
    opt((comment ~ eol) | eol) ~> opt(nonGroupFields) ~ userAgents ~ fields <~ opt(eol) ^^ {
      case ngfs ~ uas ~ fields =>
        (RobotsEntry(uas, fields.filter(_.isRight).map(f => f.right.get)),
          ngfs.getOrElse(List()) ::: fields.filter(_.isLeft).map(f => f.left.get))
    }

  def file: Parser[RobotsFile] = opt(eol) ~> rep(entry) <~ opt(eol) ^^ {
    case entries => entries.unzip match { case (gms, ngfs) => RobotsFile(gms, ngfs.flatten) }
  }

  def parse(robots: String): Either[String,RobotsFile] = parse(file, robots) match {
    case Success(result, input) => Right(result)
    case Failure(msg, input) =>
      Left(msg)
    case Error(msg, input) =>
      Left(msg)
  }

}

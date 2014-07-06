import org.rdowinton.robots._
import org.scalatest.{FunSpec, Matchers}

class RobotsSpec extends FunSpec with Matchers {
  describe("Robots") {
    it("should correctly parse a robots.txt file with more than one group") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Example"),
            List(
              Allow("*"),
              Disallow("/secret"),
              Disallow("/hidden")
            )
          ),
          RobotsEntry(
            List("Crawler"),
            List(
              Allow("/public"),
              Disallow("*")
            )
          )
        ),
        List()
      )
      Robots.parse(robotsFile.toString) should be(Right(robotsFile))
    }

    it("should correctly parse a robots.txt with no Allow or Disallow") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Example"),
            List()
          )
        ),
        List()
      )
      Robots.parse(robotsFile.toString) should be(Right(robotsFile))
    }


    it("should correctly parse a robots.txt which has a User-agent entry containing a space") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test User Agent"),
            List(
              Allow("*"),
              Disallow("/hidden")
            )
          )
        ),
        List()
      )
      Robots.parse(robotsFile.toString) should be(Right(robotsFile))
    }

    it("should correctly parse a robots.txt which has Disallow before Allow") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Disallow("*"),
              Allow("/shared")
            )
          )
        ),
        List()
      )
      Robots.parse(robotsFile.toString) should be(Right(robotsFile))
    }

    it("should correctly parse a robots.txt which has non-group fields") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Allow("/public"),
              Allow("/forums"),
              Disallow("/secret")
            )
          )
        ),
        List(
          NonGroupField("Sitemap", "http://some.example/sitemap.xml"),
          NonGroupField("Host", "some.example")
        )
      )
      Robots.parse(robotsFile.toString) should be(Right(robotsFile))
    }

    it("should correctly parse a robots.txt where non-group fields occur before or after a group") {
      val robotsTxt =
      """
        |Sitemap: http://some.example/sitemap.xml
        |Host: some.example
        |
        |
        |User-Agent: Test-Agent
        |
        |Allow: *
        |Disallow: /login
        |Test: Some value
      """.stripMargin
      val robotsFile = RobotsFile(List(
        RobotsEntry(
          List("Test-Agent"),
          List(
            Allow("*"),
            Disallow("/login")
          )
        )),
        List(
          NonGroupField("Sitemap", "http://some.example/sitemap.xml"),
          NonGroupField("Host", "some.example"),
          NonGroupField("Test", "Some value")
        )
      )
      Robots.parse(robotsTxt) should be(Right(robotsFile))
    }

    it("should correctly parse a robots.txt file where non-group fields occur within a group") {
      val robotsTxt =
      """
        |Sitemap: http://some.example/sitemap.xml
        |Host: some.example
        |
        |User-Agent: Test-Agent
        |
        |Allow: *
        |Test: Some value
        |Disallow: /login
      """.stripMargin
      val robotsFile = RobotsFile(List(
        RobotsEntry(
          List("Test-Agent"),
          List(
            Allow("*"),
            Disallow("/login")
          )
        )),
        List(
          NonGroupField("Sitemap", "http://some.example/sitemap.xml"),
          NonGroupField("Host", "some.example"),
          NonGroupField("Test", "Some value")
        )
      )
      Robots.parse(robotsTxt) should be(Right(robotsFile))
    }
  }

  describe("RobotsFile") {
    it("should disallow visiting of a path if an applicable disallow is longer than any applicable allows") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Allow("*"),
              Disallow("/secret")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/secret") should be(false)
    }

    it("should allow visiting of a path if an applicable allow is longer than any applicable disallows") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Allow("/secret/exception"),
              Disallow("/secret")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/secret/exception") should be(true)
    }

    it("should allow visiting of a path if an applicable allow is longer than an applicable wildcard disallow") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Allow("/public"),
              Disallow("*")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/public") should be(true)
    }

    it("should allow visiting of a path if an applicable allow ends with $ and the URL's end matches") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Disallow("*"),
              Allow("/test.php$")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/test.php") should be(true)
      robotsFile.canVisit("Test-Agent", "/test.php?") should be(false)
    }

    it("should allow visiting of a path if there are no allows as long as there are no matching disallows") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List()
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/random-path") should be
    }

    it("should disallow visiting of a path if there are no allows as long as there is a matching disallow") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Disallow("/secret")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/secret") should be(false)
    }

    it("should be able to match using a wildcard user-agent value") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-*"),
            List(
              Disallow("/secret")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/secret") should be(false)
    }

    it("should be able to match using wildcard allow/disallow values") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Disallow("*"),
              Allow("/public/*")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/some/path") should be(false)
      robotsFile.canVisit("Test-Agent", "/public/example") should be(true)
    }

    it("should be able to match using wildcard allow values") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Allow("*"),
              Disallow("/login")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/some/path") should be(true)
      robotsFile.canVisit("Test-Agent", "/login") should be(false)
    }

    it("should be able to match a longer path using a shorter path") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Disallow("*"),
              Allow("/public")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/some/path") should be(false)
      robotsFile.canVisit("Test-Agent", "/public/example") should be(true)
    }

    it("should allow if matched by trailing slash") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Disallow("/"),
              Allow("/test/")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/test") should be(false)
      robotsFile.canVisit("Test-Agent", "/test/example") should be(true)
    }

    it("should allow if disallow portion only applies at the end of the string") {
      val robotsFile = RobotsFile(
        List(
          RobotsEntry(
            List("Test-Agent"),
            List(
              Allow("/example*"),
              Disallow("/example123$")
            )
          )
        ),
        List()
      )
      robotsFile.canVisit("Test-Agent", "/example123") should be(false)
      robotsFile.canVisit("Test-Agent", "/example1234") should be(true)
    }
  }
}

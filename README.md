Robots parser
=============
A robots.txt parser written in Scala.

Example usage
-------------
```scala
def canVisit(robots: String, userAgent: String, path: String): Boolean = {
    Robots.parse(robots) match {
        case Right(robotsFile) => robotsFile.canVisit(userAgent, path)
        case Left(error) => {
            println("Couldn't parse robots.txt: " + error)
            false
        }
    }
}

canVisit("User-Agent: Crawler\r\nAllow: *\r\nDisallow: /login\r\n", "Crawler", "/login")
canVisit("User-Agent: Crawler\r\nAllow: *\r\nDisallow: /login\r\n", "Crawler", "/example")
```
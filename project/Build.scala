import aether._
import sbt._
import sbt.Keys._
import xml.Group

object Build extends sbt.Build {

  val antiXMLversion = "0.5.1"

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "no.arktekk.atom",
    scalaVersion := "2.10.0",
    crossScalaVersions := Seq("2.9.2", "2.9.1", "2.10.0"),
    scalacOptions := Seq("-deprecation"),
    publishTo <<= (version) apply {
      (v: String) => if (v.trim().endsWith("SNAPSHOT")) Some(Resolvers.sonatypeNexusSnapshots) else Some(Resolvers.sonatypeNexusStaging)
    },
    pomIncludeRepository := { x => false },
    credentials += Credentials(Path.userHome / ".sbt" / "arktekk-credentials")
  ) ++ Aether.aetherPublishSettings

  lazy val root = Project(
    id = "scala-atom",
    base = file("."),
    settings = buildSettings ++ Seq(
      description := "Scala Atom",
      name := "scala-atom", 
      libraryDependencies := Seq(
        "joda-time" % "joda-time" % "2.1",
	    	"org.joda" % "joda-convert" % "1.1",
		    "no.arktekk" %% "anti-xml" % antiXMLversion,
        "org.specs2" %% "specs2" % "1.12.3" % "test"
      ),
    manifestSetting
    ) ++ mavenCentralFrouFrou
  )

  object Resolvers {
    val sonatypeNexusSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    val sonatypeNexusStaging = "Sonatype Nexus Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  }

  lazy val manifestSetting = packageOptions <+= (name, version, organization) map {
    (title, version, vendor) =>
      Package.ManifestAttributes(
        "Created-By" -> "Simple Build Tool",
        "Built-By" -> System.getProperty("user.name"),
        "Build-Jdk" -> System.getProperty("java.version"),
        "Specification-Title" -> title,
        "Specification-Version" -> version,
        "Specification-Vendor" -> vendor,
        "Implementation-Title" -> title,
        "Implementation-Version" -> version,
        "Implementation-Vendor-Id" -> vendor,
        "Implementation-Vendor" -> vendor
      )
  }

  // Things we care about primarily because Maven Central demands them
  lazy val mavenCentralFrouFrou = Seq(
    homepage := Some(new URL("http://github.com/arktekk/scala-atom/")),
    startYear := Some(2012),
    licenses := Seq(("Apache 2", new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))),
    pomExtra <<= (pomExtra, name, description) {(pom, name, desc) => pom ++ Group(
      <scm>
        <url>http://github.com/arktekk/scala-atom</url>
        <connection>scm:git:git://github.com/arktekk/scala-atom.git</connection>
        <developerConnection>scm:git:git@github.com:arktekk/scala-atom.git</developerConnection>
      </scm>
      <developers>
        <developer>
          <id>hamnis</id>
          <name>Erlend Hamnaberg</name>
          <url>http://twitter.com/hamnis</url>
        </developer>
      </developers>
    )}
  )
}

import aether._
import AetherKeys._
import sbt._
import sbt.Keys._
import xml.Group

object Build extends sbt.Build {

  val antiXMLversion = "0.3"

  lazy val buildSettings = Defaults.defaultSettings ++ Aether.aetherSettings ++ Seq(
    organization := "no.arktekk.atom",
    scalaVersion := "2.9.1",
    crossScalaVersions := Seq("2.9.1"),
    deployRepository <<= (version) apply {
      (v: String) => if (v.trim().endsWith("SNAPSHOT")) Resolvers.sonatypeNexusSnapshots else Resolvers.sonatypeNexusStaging
    },
    pomIncludeRepository := { x => false },
    aetherCredentials := {
      val cred = Path.userHome / ".sbt" / "arktekk-credentials"
      if (cred.exists()) Some(Credentials(cred)) else None
    }
  )

  lazy val root = Project(
    id = "scala-atom",
    base = file("."),
    settings = buildSettings ++ Seq(
      description := "Scala Atom",
      name := "scala-atom", 
      libraryDependencies := Seq(
        "joda-time" % "joda-time" % "2.0",
	    	"org.joda" % "joda-convert" % "1.1",
		    "com.codecommit" %% "anti-xml" % antiXMLversion,
        "org.specs2" %% "specs2" % "1.6.1" % "test"
      ),
    publish <<= Aether.deployTask.init,
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

import sbt._
import sbt.Keys._
import sbtrelease.Release._
import sbtrelease.ReleasePart
import sbtrelease.ReleaseKeys._

object AtomClient extends Build {

  val antiXMLversion = "0.3"

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "no.arktekk.atom",
    scalaVersion := "2.9.1",
    crossScalaVersions := Seq("2.9.1") // Seq("2.9.0", "2.9.1"),
  )

  lazy val root = Project(
    id = "scala-atom",
    base = file("."),
    settings = buildSettings ++ releaseSettings ++ Seq(
      description := "Scala Atom",
      name := "scala-atom", 
      libraryDependencies := Seq(
        "joda-time" % "joda-time" % "2.0",
		"org.joda" % "joda-convert" % "1.1",
		"com.codecommit" %% "anti-xml" % antiXMLversion,
        "org.specs2" %% "specs2" % "1.6.1" % "test"
      ),

      releaseProcess <<= thisProjectRef apply { ref =>
        import sbtrelease.ReleaseStateTransformations._
        Seq[ReleasePart](
          initialGitChecks,
          checkSnapshotDependencies,
          inquireVersions,
          runTest,
          setReleaseVersion,
          commitReleaseVersion,
          tagRelease,
        // Enable when we're deploying to Sonatype
  //        releaseTask(publish in Global in ref),
          setNextVersion,
          commitNextVersion
        )
      }
    )
  )
}

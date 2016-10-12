name := "pacaya-playground"

version := "0.0.0-SNAPSHOT"

organization := "me.yuhuan"

scalaVersion := "2.11.8"

publishMavenStyle := true

isSnapshot := true

scalacOptions in (Compile, doc) += "-diagrams"

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

libraryDependencies += "edu.jhu.pacaya" % "pacaya" % "3.1.3"

pomExtra :=
  <url>https://github.com/jyuhuan/pacaya-playground</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:jyuhuan/pacaya-playground.git</url>
      <connection>scm:git:git@github.com:jyuhuan/pacaya-playground.git</connection>
    </scm>
    <developers>
      <developer>
        <id>yuhuan</id>
        <name>Yuhuan Jiang</name>
        <url>http://yuhuan.me/</url>
      </developer>
    </developers>

import sbt._

class CCSTMProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "0.9.5"

  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
}

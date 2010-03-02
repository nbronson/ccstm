/* CCSTM - (c) 2009-2010 Stanford University - PPL */

import sbt._

class CCSTMProject(info: ProjectInfo) extends DefaultProject(info) {
  //val scalatest = "org.scalatest" % "scalatest" % "1.0"
  val scalatest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-SNAPSHOT"

  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

  override def outputPath = outputRootPath
}

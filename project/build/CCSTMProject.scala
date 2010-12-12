/* CCSTM - (c) 2009-2010 Stanford University - PPL */

import sbt._

class CCSTMProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = crossScalaVersionString match {
    // RC7 is the same as the release, but scalatest against the release is not
    // yet available
    case "2.8.0" => "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC7-SNAPSHOT"
    case _ => "org.scalatest" % "scalatest" % ("1.2-for-scala-" + crossScalaVersionString + "-SNAPSHOT")
  }
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

  // include the license file in the resulting JAR
  //override def mainResources = super.mainResources + "LICENSE.txt"

  // this stuff doesn't work right yet
//  val pplRepo = Resolver.sftp("ppl-sftp-repo", "ppl.stanford.edu", "/var/www/html/ppl/")
//
//  override def managedStyle = ManagedStyle.Maven
//  //val publishTo = "CCSTM Release Repository at PPL" at "http://ppl.stanford.edu/ccstm/repo-releases"
//  val publishTo = "ppl-sftp-repo" at "scp://ppl.stanford.edu/var/www/html/ppl/ccstm/repo-releases"
}

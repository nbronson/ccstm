/* CCSTM - (c) 2009-2010 Stanford University - PPL */

import sbt._

class CCSTMProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = crossScalaVersionString match {
    case "2.8.0.Beta1" => "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT"
    //case _ => "org.scalatest" % "scalatest" % ("1.0.1-for-scala-" + crossScalaVersionString + "-SNAPSHOT")
    case _ => "org.scalatest" % "scalatest" % ("1.0.1-for-scala-2.8.0.RC1-SNAPSHOT")
  }
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

  // include the license file in the resulting JAR
  //override def mainResources = super.mainResources + "LICENSE.txt"

  // 2.8.0.RC1 has specialization bugs
  override def compileOptions =
    super.compileOptions.toList ++ (if (buildScalaVersion == "2.8.0.RC1") Some(CompileOption("-no-specialization")) else None)

  // this stuff doesn't work right yet
//  val pplRepo = Resolver.sftp("ppl-sftp-repo", "ppl.stanford.edu", "/var/www/html/ppl/")
//
//  override def managedStyle = ManagedStyle.Maven
//  //val publishTo = "CCSTM Release Repository at PPL" at "http://ppl.stanford.edu/ccstm/repo-releases"
//  val publishTo = "ppl-sftp-repo" at "scp://ppl.stanford.edu/var/www/html/ppl/ccstm/repo-releases"
}

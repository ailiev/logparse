// -*-scala-*-

organization := "ailiev"

name := "logparser"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= {
  Seq(
    "org.scalatest" %% "scalatest" % "1.6.+" % "test"
//    "org.scala-tools.testing" %% "specs" % "1.6.9" % "test->default"
  )
}

ivyXML := <dependencies>
    <dependency org="org.parboiled" name="parboiled-scala" rev="1.0.2"/>
    <dependency org="joda-time" name="joda-time" rev="1.6.+"/>
    <dependency org="junit" name="junit" rev="4.8.+" conf="test"/>
</dependencies>

import sbt._ 

organization := "com.despegar"

name := "point-in-polygon"

scalaVersion := "2.11.4"

scalacOptions ++= Seq(
        "-feature",
        "-language:postfixOps",
        "-deprecation"
)

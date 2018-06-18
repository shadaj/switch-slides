name := "switch-slides"

scalaVersion := "2.12.1"

libraryDependencies += "net.java.jinput" % "jinput" % "2.0.6"
libraryDependencies += "org.hid4java" % "hid4java" % "0.5.0"

resolvers += "Funky-Repo" at "http://team846.github.io/repo"

libraryDependencies += "com.lynbrookrobotics" %% "potassium-core" % "0.1.0-124917e8"
libraryDependencies += "com.lynbrookrobotics" %% "potassium-sensors" % "0.1.0-124917e8"

libraryDependencies += "com.lynbrookrobotics" %% "funky-dashboard" % "0.3.1"

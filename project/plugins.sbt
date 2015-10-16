libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")

resolvers += Resolver.url("hoard-plugin-demo", url("https://dl.bintray.com/lrytz/sbt-hoard-demo"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbt" % "sbt-hoard" % "0.0.1")

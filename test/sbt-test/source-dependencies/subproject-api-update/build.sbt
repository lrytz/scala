//ThisBuild / logLevel         := Level.Debug

lazy val core = project
lazy val root = project.dependsOn(core)

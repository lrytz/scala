import sbt._
import Keys._

object ScriptedTestPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  object autoImport {
    val setup = taskKey[Unit]("setup scripted test")
  }
  import autoImport._

  override val projectSettings = Seq(
    scalaVersion := sys.props("plugin.scalaVersion"),
    setup := IO.copyFile(Path(sys.props("scripted.common")).asFile, baseDirectory.value / "common.sbt")
  )
}

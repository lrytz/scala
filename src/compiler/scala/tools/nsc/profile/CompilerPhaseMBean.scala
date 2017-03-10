package scala.tools.nsc.profile

trait CompilerPhaseMBean {
  def getEnterPhase: String
  def setEnterPhase(phaseName: String): Unit

  def getExitPhase: String
  def setExitPhase(phaseName: String): Unit
}

class CompilerPhase extends CompilerPhaseMBean {
  private[this] var enterPhase: String = null
  override def getEnterPhase: String = enterPhase
  override def setEnterPhase(phaseName: String): Unit = { enterPhase = phaseName }

  private[this] var exitPhase: String = null
  override def getExitPhase: String = exitPhase
  override def setExitPhase(phaseName: String): Unit = { exitPhase = phaseName }
}

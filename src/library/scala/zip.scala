package scala

class zip extends StaticAnnotation

/**
 * T: top type
 * E: element type of this location
 * P: parent location type
 */
class Loc[T, E, P <: Loc[T, _, _]](v: E, f: E => Option[P]) {
  def up: Option[P] = f(v)
  def top: T = up match {
    case Some(p) => p.top
    case _ => v.asInstanceOf[T]
  }
  def copy(v: E): Loc[T, E, P] = new Loc[T, E, P](v, f)
  def set(v: E) = copy(v).top
}

package bon.jo.phy

object Purpose {


  case object PutSun extends Purpose

  case object PlanetTarget extends Purpose

  case object Move extends Purpose
  case object Delete extends Purpose
  case object Create extends Purpose

  def apply(puport : String): Purpose = all.find(_.toString == puport).get

  val all = List(PutSun,PlanetTarget,Move,Delete,Create)
}

sealed  class Purpose
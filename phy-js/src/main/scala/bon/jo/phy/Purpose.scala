package bon.jo.phy

object Purpose {

  case object PutSun extends Purpose

  case object PlanetTarget extends Purpose

  case object Move extends Purpose

}

sealed trait Purpose {

}
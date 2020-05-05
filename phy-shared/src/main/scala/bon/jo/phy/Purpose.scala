package bon.jo.phy

object Purpose {




  case object PlanetTarget extends Purpose

  case object Move extends Purpose
  case object Delete extends Purpose
  case object Create extends Purpose
  case object Void extends Purpose with What
  def apply(puport : String): Purpose = all.find(_.toString == puport).get

  val all = List(PlanetTarget,Move,Delete,Create,Void)
  object What{
    case object Point extends What
    case object Interaction extends What

  }
  sealed trait  What
}

sealed  class Purpose
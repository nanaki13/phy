package bon.jo.phy

object Purpose {




  case object PlanetTarget extends Purpose

  case object Move extends Purpose
  case object Delete extends Purpose
  case object DontFollow extends Purpose
  case object Create extends Purpose
  case object Void extends Purpose with What
  def apply(puport : String): Purpose = all.find(_.toString == puport).get

  val all = List(PlanetTarget,Move,Delete,Create,Void,DontFollow)
  object What{
    case object Point extends What
    case object Interaction extends What{
      object Type{
        def apply(s : String): Type = all(s)
      }
      sealed trait Type{
        def name : String
        def  factor : Double
        def unary_! : Type = {
          this match {
            case Attractive => Repulsive
            case Repulsive => Attractive
          }
        }
      }
      case object Attractive extends Named("Attractive",1D)  with Type
      case object Repulsive extends Named("Repulsive",-1D)  with Type
      val all: Map[String,Type] = List[Type](Attractive,Repulsive).map {e => (e.name,e)}.toMap
    }

  }
  sealed trait  What
  abstract class Named(val name : String,val factor : Double)
}

sealed  trait Purpose
package bon.jo.phy

case class Model[A <: PointDynamic](var points: List[A], var interactions : List[(A,Interaction)])

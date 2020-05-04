package bon.jo.phy

case class Model[A](var rds: List[A],var interactions : List[(PointDynamic,Interaction)])

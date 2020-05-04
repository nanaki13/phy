package bon.jo.phy

import bon.jo.phy.view.Shape
import  bon.jo.phy.PointDynamic

case class ActionPointDynamic[S <: Shape,P <: PointDynamic](p: P, what: Purpose)

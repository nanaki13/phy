package bon.jo.phy

import bon.jo.phy.view.{PointDynamicColor, Shape}

case class ActionPointDynamic[S <: Shape](p: PointDynamicColor[S], what: Purpose)

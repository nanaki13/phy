package bon.jo.phy

import bon.jo.phy.Purpose.What

case class Model[A <: PointDynamic](var points: List[A], var interactions : List[PointInteraction[A]])

case class PointInteraction[A <: PointDynamic](p : A,var interaction: Interaction,_type : What.Interaction.Type = What.Interaction.Attractive)

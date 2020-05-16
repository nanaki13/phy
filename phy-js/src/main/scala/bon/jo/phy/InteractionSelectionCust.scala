package bon.jo.phy

case class InteractionSelectionCust(override val selected: Option[PointInteraction[PointDynamicColorCircle]]) extends InteractionSelection[PointDynamicColorCircle, PointInteraction[PointDynamicColorCircle]](selected)

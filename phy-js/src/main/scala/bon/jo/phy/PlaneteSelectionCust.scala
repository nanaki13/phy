package bon.jo.phy

case class PlaneteSelectionCust(override val selected: Option[PointDynamicColorCircle]) extends PlaneteSelection[PointDynamicColorCircle](selected) {
  override def cp(): PlaneteSelectionCust = copy(selected = Some(new PointDynamicColorCircle(selected.get)))
}

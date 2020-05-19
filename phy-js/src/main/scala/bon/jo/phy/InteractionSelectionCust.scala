package bon.jo.phy

import bon.jo.phy.view.PointDynamicColor
import bon.jo.phy.view.Shape.Circle

case class InteractionSelectionCust(override val selected: Option[PointInteraction[PointDynamicColorCircle]]) extends InteractionSelection[PointDynamicColorCircle, PointInteraction[PointDynamicColorCircle]](selected){
  override def cp(): InteractionSelectionCust = copy(selected = Some(selected.get.copy(p = new PointDynamicColorCircle( selected.get.p))))
}

package bon.jo.phy

import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{Drawer, DrawerJS, PointDynamicColor}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

class PointDynamicColorCircle(mIni: Double, pIni: P, vIni: V = V(), aIni: A = A(), val colorIni: Color, val shapeIni: Circle,id : Int)
  extends PointDynamicColor[Circle](mIni, pIni, vIni, aIni, colorIni, shapeIni,id) with WithId
{
  override def mask(implicit tx: CanvasRenderingContext2D, sizeFactor: Double): Unit = {

    drawFill[Circle](this.shape * 1.2F, p)
  }

  def this(p: PointDynamicColorCircle) {
    this(p.m, p.p.copy(), p.v.copy(), p.a.copy(), p.c, p.shape.copy(),p.id)
  }

  def toJs: Null = null

  override implicit val drawer: Drawer[CanvasRenderingContext2D, Circle] = DrawerJS.CircleDraw
}

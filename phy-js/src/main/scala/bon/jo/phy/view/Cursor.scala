package bon.jo.phy.view


import java.util.UUID

import bon.jo.Logger
import bon.jo.html.Types.FinalComponent
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import bon.jo.html.DomShell.{$, ExtendedElement}
import bon.jo.phy.ImportExport.PointExport
import bon.jo.phy.Obs
import bon.jo.phy.Phy.P
import bon.jo.phy.view.Cursor.CursorParam
import org.scalajs.dom.ext.Color

import scala.xml.Node

class Cursor extends FinalComponent[Div] with CursorParam {
  def bindBackgroundRed(): Obs[Color] = newNormalizedValue.map {
    e =>
      val c = Color(math.round(e.x * 255d).toInt, 0, 0)
      me.style.backgroundColor = c.toHex
      c
  }

  def bindBackgroundGreen(): Obs[Color]= newNormalizedValue.map {
    e =>
      val c = Color(0, math.round(e.x * 255d).toInt, 0)
      me.style.backgroundColor = c.toHex
      c
  }

  def bindBackgroundBlue(): Obs[Color] = newNormalizedValue.map {
    e =>
      val c = Color(0, 0, math.round(e.x * 255d).toInt)
      me.style.backgroundColor = c.toHex
      c
  }


  def noXDelta: Cursor = {
    _noXDelta = true
    this
  }

  def noYDelta: Cursor = {
    _noYDelta = true
    this
  }

  val newNormalizedValue: Obs[P] = Obs.once()

  override def xml(): Node = {
    <div id={id} class="cursor">
      <div id={id + "-cursor"} class="the-cursor d-inline">X</div>
    </div>
  }


  def update(e : MouseEvent) = {
    val c: Div = $[Div](id + "-cursor")
    val size = P(c.getBoundingClientRect().right - c.getBoundingClientRect().left, c.getBoundingClientRect().bottom - c.getBoundingClientRect().top)
    val sizeCont = P(me.getBoundingClientRect().right - me.getBoundingClientRect().left, me.getBoundingClientRect().bottom - me.getBoundingClientRect().top)
    val clk = P(e.clientX, e.clientY) - P(me.getBoundingClientRect().left, me.getBoundingClientRect().top)
    val center = clk - (size / 2)
    if (!_noYDelta) c.style.top = (center.y) + "px"
    if (!_noXDelta) c.style.left = (center.x) + "px"
    newNormalizedValue.newValue(P(clk.x / sizeCont.x, clk.y / sizeCont.y))
  }
  override def init(parent: HTMLElement): Unit = {

  //  me.clkOnce().suscribe(update)
    me.addEventListener("drag",update)
    me.addEventListener("dragend",update)
    me.addEventListener("dragstart",update)
    me.addEventListener("click",update)
  }

  override val id: String = {
    UUID.randomUUID().toString
  }
}

object Cursor {

  trait CursorParam {
    protected var _noXDelta = false
    protected var _noYDelta = false
  }

  def xCursor = {
    (new Cursor).noYDelta
  }

  def yCursor = {
    (new Cursor).noXDelta
  }
}
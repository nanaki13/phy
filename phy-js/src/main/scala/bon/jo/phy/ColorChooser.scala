package bon.jo.phy

import bon.jo.Logger
import bon.jo.html.Types.FinalComponent
import bon.jo.phy.view.Cursor
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw

import scala.xml.Elem

class ColorChooser extends FinalComponent[Div] with AutoId {

  def xml(): Elem = <div id={id}>
    {cursorRed.xml()}{cursorGreen.xml()}{cursorBlue.xml()}
  </div>

  val cursorRed: Cursor = Cursor.xCursor
  val cursorGreen: Cursor = Cursor.xCursor
  val cursorBlue: Cursor = Cursor.xCursor

  var sommeColr = Option.empty[Color]
  val colorObs : Obs[Color] = Obs.once().toMany



  private def emit(c : Color ): Color = {colorObs.newValue(c);c}
  def updateRedColor(c: Color): Unit = {
    sommeColr = Option(sommeColr.map(o => Color(0, o.g, o.b)).getOrElse(Color(0, 0, 0)) + c).map(emit)
  }

  def updateBlueColor(c: Color): Unit = {
    sommeColr = Option(sommeColr.map(o => Color(o.r, o.g, 0)).getOrElse(Color(0, 0, 0)) + c).map(emit)
  }

  def updateGreenColor(c: Color): Unit = {
    sommeColr = Option(sommeColr.map(o => Color(o.r, 0, o.b)).getOrElse(Color(0, 0, 0)) + c).map(emit)
  }

  def init(root: raw.HTMLElement): Unit = {
    cursorRed.init(root)
    cursorRed.bindBackgroundRed().suscribe(updateRedColor)
    cursorBlue.init(root)
    cursorBlue.bindBackgroundBlue().suscribe(updateBlueColor)
    cursorGreen.init(root)
    cursorGreen.bindBackgroundGreen().suscribe(updateGreenColor)
    colorObs.suscribe(v => {
      cursorRed.me.parentElement.style.backgroundColor = v.toHex
    })
  }


}

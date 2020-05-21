package bon.jo.phy

import bon.jo.Logger
import bon.jo.html.Types.FinalComponent
import bon.jo.phy.view.{Cursor, RGB}
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw

import scala.xml.Elem

class ColorChooser(initialColor: Color) extends FinalComponent[Div] with AutoId {

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
    sommeColr = Option(sommeColr.map(o => o * Color(0, 1, 1)).getOrElse(initialColor* Color(0, 1, 1)) + c).map(emit)
  }

  def updateGreenColor(c: Color): Unit = {
    sommeColr = Option(sommeColr.map(o => o * Color(1, 0, 1)).getOrElse(initialColor* Color(1, 0, 1)) + c).map(emit)
  }
  def updateBlueColor(c: Color): Unit = {
    sommeColr = Option(sommeColr.map(o => o * Color(1, 1, 0)).getOrElse(initialColor* Color(1, 1, 0)) + c).map(emit)
  }


  def init(root: raw.HTMLElement): Unit = {
    cursorRed.init(root)
    cursorRed.bindBackgroundRed().suscribe(updateRedColor)
    cursorBlue.init(root)
    cursorBlue.bindBackgroundBlue().suscribe(updateBlueColor)
    cursorGreen.init(root)
    cursorGreen.bindBackgroundGreen().suscribe(updateGreenColor)
    cursorRed.setColor(initialColor,RGB.R)
    cursorGreen.setColor(initialColor,RGB.G)
    cursorBlue.setColor(initialColor,RGB.B)
    cursorRed.me.parentElement.style.backgroundColor = initialColor.toHex
    colorObs.suscribe(v => {
      cursorRed.me.parentElement.style.backgroundColor = v.toHex
    })
    sommeColr = Some(initialColor)
  }


}

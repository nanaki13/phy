package bon.jo.phy

import bon.jo.phy.MainGalaxy.Model
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.view.DrawerJS._
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{DrawerJS, PointDynamicColor}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

import scala.scalajs.js.timers.SetIntervalHandle
import scala.util.Random


object MainGalaxy extends App {
  // val a = A(1,1)
  val a0 = 0
  val a_0 = A(0, 0)
  val v_0 = V(70, 0)
  val p_0 = P(600, 350)
  val dt = 1F / 50F
  val dekataTFrame = 1F / 20F

  var tmp: Option[Seq[(P, V)]] = None

  def draw(e: P, vi: V)(implicit ctx: CanvasRenderingContext2D): Unit = {
    ctx.beginPath()
    ctx.strokeStyle = "black"
    ctx.moveTo(e.x, e.y)
    ctx.lineTo(e.x + vi.x, e.y + vi.y)
    ctx.stroke()
  }

  case class Model(var rds: List[PointDynamicColor[Circle]])


  private var t = 0
  private var initPhase = true

  private var mainAnim: SetIntervalHandle = _

  def stop(): Unit = {
    scalajs.js.timers.clearInterval(mainAnim)
  }


  def drawFirst()(implicit ctx: CanvasRenderingContext2D, uIParams: UIParams): Unit = {
    import uIParams._
    implicit val s: Double = sizeFactor
    (new PointDynamicColorImpl(1d, P(minViewX, minViewY), c = Color.Black, shape = Circle(20))).draw
    uIParams.forSun { soleilEl => {
      soleilEl.asInstanceOf[PointDynamicColor[Circle]].draw
      ctx.strokeStyle = "red"
      DrawerJS.CircleDraw.drawStrike(Circle(uIParams.rSup), soleilEl.p)(ctx, 1)
      ctx.strokeStyle = "green"
      DrawerJS.CircleDraw.drawStrike(Circle(uIParams.rInf), soleilEl.p)(ctx, 1)
      (new PointDynamicColorImpl(1d, P(minViewX, minViewY), c = Color.Black, shape = Circle(20))).draw
    }
    }
  }


  def calculAndDraw(calculParam: CalculParam)(implicit ctx: CanvasRenderingContext2D
                                              , calculateur: Calculateur
                                              , uiParam: UIParams
                                              , UI: UI
  ,eventContext: EventContext): Unit = {
    implicit val s: Double = uiParam.sizeFactor
    UI.drawSun

    val sgn = if (calculParam.scleTime > 0) 1 else -1
    val dtt = sgn * dt
    val intPos = sgn * calculParam.scleTime.toInt
    val pCam = UI.camera.p
    calculateur.model.rds.foreach(e => {
      if (!uiParam.tracer) {
        ctx.fillStyle = uiParam.maskColor
        e.mask
      }

      calculParam.forSun(s =>
        for (_ <- 0 until intPos) {
          calculateur.calculnext(e, s, calculParam)
          e.addDt(dtt)
        }
      )


      tmp.foreach(e => e.foreach(draw _ tupled _))
      t += 1
    })

    if(pCam != UI.camera.p){
      UI.goTo(pCam)
    }
    calculateur.model.rds.foreach(e => e.draw)
  }


  def linkParam(calculParam: CalculParam)(implicit ui: UI, eventContext: EventContext) = {
    eventContext.correction.suscribe(calculParam.correction = _)
    //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.suscribe(calculParam.frt = _)
    eventContext.ineraction.suscribe(calculParam.interaction = _)
    eventContext.masseSolei.suscribe(calculParam.soleilMasse = _)
    eventContext.scaleTime.suscribe(calculParam.scleTime = _)
    calculParam.kRessort = 1
    eventContext.speedFactor.suscribe(calculParam.speedFactor = _)

  }

  def linkAction(calculParam: CalculParam)(implicit calculateur: Calculateur, ui: UI, uIParams: UIParams, ctx: CanvasRenderingContext2D, eventContext: EventContext) = {
    implicit val s = uIParams.sizeFactor
    eventContext.action.suscribe(action => {
      action.what match {
        case Purpose.PutSun => calculParam.sun = {
          action.p match {
            case impl: PointDynamicColorImpl => impl.draw
            case _ =>
          }
          Some(action.p);
        }
        case Purpose.PlanetTarget =>
        case Purpose.Move =>
      }
    })
    eventContext.actionPoint.suscribe(action => {
      action.what match {
        case Purpose.PutSun =>
        case Purpose.PlanetTarget =>
        case Purpose.Move => ui.goTo(action.p); ui.camera = PointDynamicImpl(action.p)
      }
    })
    eventContext.speedFactor.suscribe(calculateur.applyV)
    eventContext.stabilise.suscribe(calculateur.stab)

    eventContext.pushPull.suscribe(e => if (e) calculateur.push else calculateur.pull)
    eventContext.viewPort.suscribe(viewPort = _)
    eventContext.replaceAround.suscribe(e => {
      ui.clear
      calculateur.replaceAround(viewPort.w.x/4, 0, calculParam)
    })
    eventContext.userChoicePlanete.suscribe(e => {
      ui.follow( calculateur.model.rds(e))
    })
    eventContext
  }

  var viewPort: ViewPort = _


  def go(): Unit = {

    implicit val uiParams: UIParams = UIParams()
    implicit val ui: UI = UI()
    implicit val eventContext: EventContext = EventContext()
    implicit val ctx: CanvasRenderingContext2D = ui.getCtx2D
    ui.initView()
    // physical repère
    ctx.transform(1, 0, 0, -1, 1, 1)
    ctx.translate(0, -uiParams.height)

    implicit val m = Model(Nil)
    implicit val calculParam: CalculParam = CalculParam(uiParams)
    implicit val calcul: Calculateur = Calculateur(m)
    linkParam(calculParam)
    linkAction(calculParam)
    eventContext.viewPort.newValue(ui.viewPort)

    // org.scalajs.dom.window.addEventListener[KeyboardEvent]("keyup", hadleKeyB)
    mainAnim = scalajs.js.timers.setInterval(dekataTFrame)({
      drawFirst()

      calculAndDraw(calculParam)
      if(calcul.haveToStab){
        eventContext.stabilise.newValue(false)
      }
      if (m.rds.length < 20) {
        if (Random.nextDouble() > 0.85) {
          m.rds = m.rds :+ rdPointDynamic
          eventContext.planeteAdded.newValue( m.rds.size -1)
        }
      }

    })


  }

  def rdInt(e: Int) = Random.nextInt(e)


  def rdPointDynamic(implicit uIParams: UIParams): PointDynamicColor[Circle] = {
    val px = Random.nextInt(uIParams.width)

    val py = Random.nextInt(uIParams.height)
    val vy = Random.nextInt(3) - 1
    val vx = Random.nextInt(3) - 1
    // val vy = 0
    //    val vx = 0
    val m = 700 + 600 * Random.nextDouble()
    new PointDynamicColorImpl(m, P(px, py), V(vx, vy), A(), Color(rdInt(255), rdInt(255), rdInt(255)), Circle(m * 0.01))
  }


  go()
}

class PointDynamicColorImpl(m: Double, pIni: P, vIni: V = V(), aIni: A = A(), c: Color, shape: Circle) extends PointDynamicColor[Circle](m, pIni, vIni, aIni, c, shape) {
  override def mask(implicit tx: CanvasRenderingContext2D, sizeFactor: Double): Unit = {
    drawFill[Circle](this.shape * 1.2F, p)
  }
}



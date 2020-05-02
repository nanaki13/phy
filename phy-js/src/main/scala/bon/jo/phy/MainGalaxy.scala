package bon.jo.phy

import bon.jo.phy.Phy.{A, P, PointDynamic, V}
import bon.jo.phy.MainGalaxy.Model
import bon.jo.phy.view.DrawerJS._
import bon.jo.phy.view.Shape.Circle
import bon.jo.phy.view.{DrawerJS, PointDynamicColor}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

import scala.scalajs.js.timers.SetIntervalHandle
import scala.util.Random
import bon.jo.phy.view.DrawerJS._


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



  def drawFirst()(implicit ctx: CanvasRenderingContext2D, uIParams: UIParams with CalculParam): Unit = {
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
      //      if (soleilMasse < 500 && initPhase) {
      //        soleilMasse += 1
      //        masseSoleilInput.me.innerText = soleilMasse.toString
      //      } else {
      //        initPhase = false
      //      }


    }
    }
  }


  def calculAndDraw(calculParam: CalculParam)(implicit ctx: CanvasRenderingContext2D, calculateur: Calculateur, uiParam: UIParams): Unit = {
    implicit val s: Double = uiParam.sizeFactor

    val sgn = if (calculParam.scleTime > 0) 1 else -1
    val dtt = sgn * dt
    val intPos = sgn * calculParam.scleTime.toInt
    calculateur.model.rds.foreach(e => {

      uiParam.forSun(s =>
        for (_ <- 0 until intPos) calculateur.calculnext(e, s)
      )

      if (!uiParam.tracer) {
        ctx.fillStyle = uiParam.maskColor
        e.mask
      }
      e.addDt(dtt)
      e.draw
      tmp.foreach(e => e.foreach(draw _ tupled _))
      t += 1
    })
  }


  def linkParam( calculParam: CalculParam)(implicit ui: UI,eventContext: EventContext) = {
    eventContext.correction.suscribe(calculParam.correction = _)
 //   eventContext.action.suscribe(calculParam.correction = _)
    eventContext.frotement.suscribe(calculParam.frt = _)
    eventContext.ineraction.suscribe(calculParam.interaction = _)
    eventContext.masseSolei.suscribe(calculParam.soleilMasse = _)
    eventContext.scaleTime.suscribe(calculParam.scleTime = _)
    calculParam.kRessort = 1
    eventContext.speedFactor.suscribe(calculParam.speedFactor= _)

  }
  def linkAction(calculParam: CalculParam)(implicit ui: UI, ctx: CanvasRenderingContext2D,eventContext: EventContext) = {
    eventContext.action.suscribe(    action => {
      action.what match {
        case Purpose.PutSun => calculParam.sun = Some(action.p)
        case Purpose.PlanetTarget =>
        case Purpose.Move =>
      }
    })
    eventContext.actionPoint.suscribe(    action => {
      action.what match {
        case Purpose.PutSun =>
        case Purpose.PlanetTarget =>
        case Purpose.Move => ui.goTo(action.p)
      }
    })
  }

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
    val calculParam = CalculParam(uiParams)
    linkParam(calculParam)
    linkAction(calculParam)
    implicit val calcul: Calculateur = Calculateur(m)
    // org.scalajs.dom.window.addEventListener[KeyboardEvent]("keyup", hadleKeyB)
    mainAnim = scalajs.js.timers.setInterval(dekataTFrame)({
      drawFirst()
      calculAndDraw(calculParam)
      if (m.rds.length < 20) {
        if (Random.nextDouble() > 0.85) {
          m.rds = m.rds :+ rdPointDynamic

          // rds = rds.tail

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
    val m = 0.7 + 0.6 * Random.nextDouble()
    new PointDynamicColorImpl(m, P(px, py), V(vx, vy), A(), Color(rdInt(255), rdInt(255), rdInt(255)), Circle(m * 30))
  }


  go()
}

class PointDynamicColorImpl(m: Double, pIni: P, vIni: V = V(), aIni: A = A(), c: Color, shape: Circle) extends PointDynamicColor[Circle](m, pIni, vIni, aIni, c, shape) {
  override def mask(implicit tx: CanvasRenderingContext2D, sizeFactor: Double): Unit = {
    drawFill[Circle](this.shape * 1.2F, p)
  }
}

case class Calculateur(model: Model) {


  def rds: Seq[PointDynamicColor[Circle]] = model.rds

  def replaceAround(from: Double, step: Double)(implicit calculParam: CalculParam): Unit = {
    calculParam.forSun(sun => {
      rds.zipWithIndex.foreach(planeteIndex => {
        val (pl, i) = planeteIndex
        val distToSun = i * step + from
        val angle = Random.nextDouble() * `2PI`
        val newP = P(sun.x + scala.math.cos(angle) * distToSun, sun.y + scala.math.sin(angle) * distToSun)
        val newV = V(-Math.sqrt(2) * Math.sqrt(distToSun) * scala.math.sin(angle), Math.sqrt(2) * Math.sqrt(distToSun) * scala.math.cos(angle))

        pl.p = newP
        pl.v = newV
      })
    })
  }

  def stab(implicit calculParam: CalculParam) =  calculParam.forSun(sun => {
    rds.foreach(pl => {
      val distToSun = sun.p - pl.p
      val n = distToSun.n
      val uniBase = (new V(distToSun / distToSun.n))
      val uni = uniBase.rotate90
      pl.v = uni * 2 * Math.sqrt(pl.v.n)


    })
  }
  )

  def push = rds.foreach(p => p.v = new V(p.a / p.a.n * p.v.n))

  def pull = rds.foreach(p => p.v = -(new V(p.a / p.a.n * p.v.n)) * 1.2f)

  def applyV(factorV: Double) = {
    rds.foreach(p => p.v = p.v * factorV)
  }

  def turnAroundSun(p: P) = {

    rds.foreach(e => {
      val vect: V = new V(p - e.p)
      val vi = ((vect.rotate90) / vect.n) * e.v.n

      e.v = vi
      e.a = A()
      (e.p.copy(), vi.copy())
    })

  }

  implicit val caculContext: CaculContext = CaculContext()

  def calculnext(e: PointDynamic, s: PointDynamic)(implicit calculParam: CalculParam): Unit = {

    val soleil = s.p
    val dir = soleil - e.p

    val dist = dir.n

    caculContext.point = e
    caculContext.pointToSun = new A(dir / dist)
    caculContext.distCarre = dir.n2
    caculContext.dist = Math.sqrt(dir.n2)
    e.a = calculParam.interaction.resultatA

  }
}

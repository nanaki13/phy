package bon.jo.phy

import bon.jo.Logger
import bon.jo.html.DomShell.{$, ExtendedElement}
import bon.jo.html.Types.FinalComponent
import bon.jo.html.{DomShell, InDom, XmlHtmlView}
import bon.jo.phy.Phy.{A, P, V}
import bon.jo.phy.Purpose.Delete
import bon.jo.phy.view.DrawerJS._
import bon.jo.phy.view.{PointDynamicColor, Shape, UIParams, ViewPort}
import bon.jo.phy.view.Shape.Circle
import org.scalajs.dom.{CanvasRenderingContext2D, Event}
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, KeyboardEvent}
import org.scalajs.dom.html.Select
import org.scalajs.dom.html.{Option => OptHtml}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Group, Node}

case class UI()(implicit uIParams: UIParams) {

  var camera  = PointDynamicImpl(P(uIParams.width/2,uIParams.height/2),V(),A(),0)
  def follow(value: PointDynamicColor[_ <: Shape])(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext): Unit = {
   scalajs.js.special.debugger()
    camera = value
    goTo(camera.p)
  }


  import uIParams._

  var viewPort: ViewPort = view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale))

  def clear(implicit ctx: CanvasRenderingContext2D) = {

    ctx.fillStyle = maskColor
    ctx.fillRect(viewPort.leftBottm.x, viewPort.leftBottm.y, viewPort.w.x, viewPort.h.y)
    ctx.strokeStyle = "black"
    ctx.strokeRect(viewPort.leftBottm.x, viewPort.leftBottm.y, (viewPort.w.x - 5), (viewPort.h.y - 5))
  }

  def drawSun(implicit ctx: CanvasRenderingContext2D, sizeFactor: Double) = {
    sunCircle foreach (e => e.draw)
  }

  def initView()(implicit ctx: CanvasRenderingContext2D, eventsHandler: EventContext): EventContext = {
    addHtmlAndEvent
  }

  def goTo(dest: P)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext, eventsHandler: EventContext): Unit = {

    val current = P(minViewX + (width / 2) / scale, minViewY + (height / 2) / scale)
    val p = dest - current
    ctx.translate(-p.x, -p.y)
    minViewX = minViewX + p.x
    minViewY = minViewY + p.y
    eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))

  }

  def getCtx2D(implicit eventContext: EventContext): CanvasRenderingContext2D = {
    val cn = InDom[Div](<div id="cnt-canvas" tabindex="0"></div>)

    org.scalajs.dom.document.body.appendChild(cn.html())

    cn.me.appendChild(canvas.html())
    implicit val ct = canvas.me.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    cn.me.addEventListener[KeyboardEvent]("keypress", hadleKeyB)
    ct
  }

  def addHtmlAndEvent(implicit ctx: CanvasRenderingContext2D, eventsHandler: EventContext): EventContext = {

    def apply[A](): Obs[A] = Obs.once[A]()

    implicit val e: () => Obs[String] = apply _

    mainBag.cellByRaw = 5
    mainBag.addCell(bagName("temps * ", timeup.xml()))
    mainBag.addCell(keepTail.xml())
    mainBag.addCell(bagName("vitesse planéte * ", speedUp.xml()))
    mainBag.addCell(comeBack.xml())
    mainBag.addCell(partirBack.xml())
    mainBag.addCell(effacer.xml())
    mainBag.addCell(bagName("masse du soliel : ", masseSoleilInput.xml()))
    mainBag.addCell(bagName("interaction:", interactionType.xml()))
    mainBag.addCell(bagName("frotement:", frtOptionIn.xml()))

    val correctionBag = bagName("correction:", correctionInput.xml())
    mainBag.addCell(correctionBag)
    val cIdom = InDom[Div](correctionBag)
    var b = bagName("taille:", sizeFactorInput.xml())
    mainBag.addCell(b)

    b = bagName("masse variation:", vMas.xml())
    mainBag.addCell(b)
    mainBag.addCell(stabilise.xml())
    mainBag.addCell(toSun.xml())
    mainBag.addCell(replacer.xml())
    b = bagName("Planètes :", chiocePlanete.xml())
    mainBag.addCell(b)

    b = bagName("Action :", planeteAction.xml())
    mainBag.addCell(b)
    org.scalajs.dom.document.body.appendChild(root.html())
    val unserInput = masseSoleilInput.me.UserCanUpdate()
    unserInput.suscribe(e => {
      eventsHandler.masseSolei.newValue(e.toDouble)
    })


    keepTail.me.clkOnce().suscribe(e => {
      tracer = !tracer
      eventsHandler.tracer.newValue(tracer)
      val tracerString = if (tracer) "oui" else "non"
      keepTail.me.innerText = s"tracter:${tracerString}"
    })


    val tUpUpdate = timeup.me.UserCanUpdate()

    tUpUpdate.suscribe(e => {
      eventsHandler.scaleTime.newValue(e.toDouble)
    })
    sizeFactorInput.me.UserCanUpdate().suscribe(e => {
      eventsHandler.sizeFactor.newValue(e.toDouble)
    })


    pMas.clkOnce().suscribe(e => {
      soleilMasse *= 1.1
      eventsHandler.soleilMasse.newValue(soleilMasse)
      masseSoleilInput.me.innerText = soleilMasse.toString
    })
    mMas.clkOnce().suscribe(e => {
      soleilMasse *= 0.9
      eventsHandler.soleilMasse.newValue(soleilMasse)
      masseSoleilInput.me.innerText = soleilMasse.toString
    })
    toSun.me.clkOnce().suscribe(e => {
      //gotTo
      sunCircle.foreach(s => {
        eventsHandler.actionPoint.newValue(ActionPoint(s.p, Purpose.Move))
      })
    })
    speedUp.me.UserCanUpdate().suscribe(e => {
      eventsHandler.speedFactor.newValue(e.toDouble)

    })
    comeBack.me.clkOnce().suscribe(e => {
      eventsHandler.pushPull.newValue(false)
    })
    partirBack.me.clkOnce().suscribe(e => {
      eventsHandler.pushPull.newValue(true)
    })
    effacer.me.clkOnce().suscribe(e => {
      eventsHandler.clean.newValue(())
      //   centreG.p.map(_.p).foreach(turnAroundSun)
    })
    interactionType.me.clkOnce().suscribe(e => {
      switchIneraction = switchIneraction.tail :+ switchIneraction.head
      interaction = switchIneraction.head
      interactionType.me.innerText = interaction.name
      eventsHandler.ineraction.newValue(interaction)
    })


    frtOptionIn.me.UserCanUpdate().suscribe(e => {
      eventsHandler.frotement.newValue(e.trim.toDouble)
    })


    cIdom.me.clkOnce().suscribe(e => {
      correction = !correction
      correctionInput.me.innerText = if (correction) "Oui" else "Non"
      eventsHandler.correction.newValue(correction)
    })
    replacer.me.clkOnce().suscribe(e => {
      eventsHandler.replaceAround.newValue(())

    })
    plneteSelection.addEventListener[Event]("change", e => {
      eventsHandler.userChoicePlanete.newValue(e.target.asInstanceOf[Select].value.toInt)
    })


    var selectedPurpose : Purpose = Delete
    planeteActionRef.addEventListener[Event]("change", e => {
      Logger.log(planeteActionRef.value)
      selectedPurpose = Purpose(planeteActionRef.value)
    })
    planeteActionSubmit.addEventListener[Event]("click", e => {
      eventsHandler.userWant.newValue(selectedPurpose)
    })

    trait ObsViewUpdateText[E] {
      val obs: Obs[E]
      val htmlCp: InDom[Div]

      def text(e: E): String

      def link() = obs.suscribe(e => {
        htmlCp.me.innerText = text(e)
      })
    }
    object ObsViewUpdateText {
      def apply[E](obsp: Obs[E], htmlCpp: InDom[Div], textF: E => String): ObsViewUpdateText[E] = {
        new ObsViewUpdateText[E]() {
          override val obs: Obs[E] = obsp
          override val htmlCp: InDom[Div] = htmlCpp

          override def text(e: E): String = textF(e)
        }
      }
    }
    var stabiliseV = false
    stabilise.me.clkOnce().suscribe(e => {
      if (!stabiliseV) {
        stabiliseV = true
        eventsHandler.stabilise.newValue((stabiliseV))
      }

    })

    ObsViewUpdateText.apply(eventsHandler.stabilise, stabilise, (b: Boolean) => {
      stabiliseV = b
      "Stabilise" + (if (b) "Oui" else "Non")
    }).link()
    eventsHandler.stabilise.newValue(false)
    canvas.me.clkOnce().suscribe(e => {
      val rect = canvas.me.getBoundingClientRect()
      val xc = rect.left
      val yc = rect.bottom
      val pc = P(xc, yc)

      val x = e.clientX
      val y = -e.clientY
      val clickIn = pc + P(x, y)
      val other = P((clickIn.x / scale + minViewX), (clickIn.y / scale + minViewY))


      implicit val s: Double = sizeFactor
      sunCircle.foreach { soleilEl => {
        ctx.fillStyle = maskColor
        soleilEl.mask
      }
      }
      sunCircle = Some(
        new PointDynamicColorCircle(soleilMasse, other, V(), A(), Color("#FF5F1C"), Circle(20f))
      )

      eventsHandler.action.newValue(ActionPointDynamic(sunCircle.get, Purpose.PutSun))

    })

    eventsHandler.planeteAdded.suscribe {
      addForChoice
    }
    eventsHandler.planeteRemove.suscribe {
      removeFromSelection
    }
    eventsHandler
  }


  var planeteIndex = List[Int]()
  def addForChoice(i : Int): Unit = {
    planeteIndex = planeteIndex :+ i
    val opt_ = opt( i)
    plneteSelection.appendChild(opt_.html())
  }
  def opt(i : Int) = InDom[OptHtml]( <option value={i.toString}>{i.toString}</option>)
  def removeFromSelection(i : Int): Unit = {
    planeteIndex = planeteIndex.tail.zipWithIndex.map(_._2)

    plneteSelection.clear()
    planeteIndex.foreach(e => plneteSelection.appendChild(opt(e).html()))
  }

  def dw(implicit uIParams: UIParams) = (-uIParams.width / 4d)

  def dh(implicit uIParams: UIParams) = (-uIParams.height / 4d)

  def hadleKeyB(e: KeyboardEvent)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext, uIParams: UIParams) = {


    import uIParams._
    val keysMapProcess = Map(
      "+" -> (() => {

        ctx.translate((width / 2) * scale, (height / 2) * scale)
        minViewX -= (width / 2) * scale
        minViewY -= (height / 2) * scale
        scale = scale * 1.1
        minViewX /= 1.1
        minViewY /= 1.1
        ctx.scale(1.1, 1.1)
        ctx.translate(-(width / 2) * scale, -(height / 2) * scale)
        minViewX += (width / 2) * scale
        minViewY += (height / 2) * scale
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))

      }), "-" -> (() => {


        ctx.translate((width / 2) * scale, (height / 2) * scale)
        minViewX -= (width / 2) * scale
        minViewY -= (height / 2) * scale
        scale = scale * 0.9
        minViewX /= 0.9
        minViewY /= 0.9
        minViewX += (width / 2) * scale
        minViewY += (height / 2) * scale
        ctx.scale(0.9, 0.9)
        ctx.translate(-(width / 2) * scale, -(height / 2) * scale)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))

      }), "d" -> (() => {
        val v = dw
        minViewX += v
        ctx.translate(-v, 0)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "q" -> (() => {
        val v = dw
        minViewX -= v
        ctx.translate(v, 0)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "z" -> (() => {
        val v = dh
        minViewY += v
        ctx.translate(0, -v)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "s" -> (() => {
        val v = dh
        minViewY -= v
        ctx.translate(0, v)
        eventContext.viewPort.newValue(view.ViewPort(scale, P(minViewX, minViewY), V(width / scale), V(0, height / scale)))
      }), "p" -> (() => {
        sizeFactor *= 1.1
        eventContext.sizeFactor.newValue(sizeFactor)
      }), "m" -> (() => {
        sizeFactor *= 0.9
        eventContext.sizeFactor.newValue(sizeFactor)
      })
    )

    keysMapProcess.get(e.key).map(e => {
      clear
      e()
      true
    }).foreach(_ => {

      clear
    })
    eventContext.viewPort.suscribe(v => {
      CircleDraw.drawFill(Circle(50), v.leftBottm + P(v.w.x / 2, v.h.y / 2))(ctx, 1)
      this.viewPort = v
    })

  }

  lazy val stabilise = InDom[Div]({
    <div id="applyEnergyEqual" class="col btn in">
      Stabilise
    </div>
  })
  lazy val canvas: XmlHtmlView[Canvas] = InDom[Canvas](<canvas style="position:absolute;top:0;z-index:0;" id="gameCanvas" width={width.toString} height={height.toString}></canvas>)

  lazy val masseSoleilInput = InDom[Div]({
    <div id="in" class="btn in col">
      {soleilMasse}
    </div>
  })

  lazy val effacer = InDom[Div](<div id="trn" class="btn in col">Effacer</div>
  )

  lazy val comeBack = InDom[Div](<div id="cmb" class="btn in col">revient</div>
  )
  lazy val partirBack = InDom[Div](<div id="push" class="btn in col">partir</div>
  )
  lazy val keepTail = InDom[Div]({
    <div id="keepTail" class="btn in col">tracter:
      {tracerString}
    </div>
  })
  lazy val speedUp = InDom[Div]({
    <div id="speeUp" class="btn in">
      {speedFactor}
    </div>
  })
  lazy val timeup = InDom[Div]({
    <div id="timeup" class="btn in">
      {scleTime}
    </div>
  })
  lazy val correctionInput = InDom[Div]({
    <div id="cor" class="btn in">
      {if (correction) "Oui" else "Non"}
    </div>
  })
  lazy val sizeFactorInput = InDom[Div]({
    <div id="sizeFactor" class="btn in">
      {sizeFactor}
    </div>
  })

  lazy val interactionType = InDom[Div]({
    <div id="inter" class="btn in">
      {switchIneraction.head.name}
    </div>
  })
  lazy val frtOptionIn = InDom[Div]({
    <div id="frt" class="btn in">0</div>
  })
  lazy val kRessortInput = InDom[Div]({
    <div id="kRessortInput" class="btn in">
      {kRessort}
    </div>
  })

  lazy val vMas = InDom[Div]({
    <div id="vMasse" class="d-inline">
      <div id="pMasse" class="btn in">
        +
      </div>
      <div id="mMasse" class="btn in">
        -
      </div>
    </div>
  })
  lazy val pMas = $[Div]("pMasse")
  lazy val mMas = $[Div]("mMasse")
  lazy val toSun = InDom[Div]({
    <div id="toSun" class="col btn in">
      Aller au soleil
    </div>
  })
  lazy val replacer = InDom[Div]({
    <div id="replacer" class="col btn in">
      Replacer Planete
    </div>
  })
  lazy val chiocePlanete = InDom[Div] (
    <div id="planeteChoice">
      <select id="pl">
      </select>
    </div>)

  lazy val planeteAction = InDom[Div] (
    <div id="planeteAction">
      <select id="planeteAction-select">
        <option value={Purpose.Delete.toString}>{Purpose.Delete.toString}</option>
        <option value={Purpose.Create.toString}>{Purpose.Create.toString}</option>
      </select>
      <div class="btn in" id ="planeteAction-ok">Appliquer</div>
    </div>)
  lazy val  planeteActionSubmit = $[Div]("planeteAction-ok")
  lazy val planeteActionRef = $[Select]("planeteAction-select")
  lazy val plneteSelection = $[Select]("pl")
  def bagName(name: String, node: Node) = {
    <div class="col d-inline" id={name + "-id"}>
      <span>
        {name}
      </span>{node}
    </div>
  }

  object mainBag extends FinalComponent[Div] {
    var cellByRaw = 5
    var current = 1

    def addCell(n: Node): Unit = {
      table.head += n
      current += 1
      if (current > cellByRaw) {
        current = 1
        table = ListBuffer[Node]() :: table
      }
    }

    var table: List[mutable.ListBuffer[Node]] = ListBuffer[Node]() :: Nil

    override def xml(): Node = {
      val rows: List[Elem] = for {r <- table.reverse} yield {
        <div class="row">
          {Group(r)}{r.size}
        </div>
      }
      <div id={id}>
        {Group(rows)}
      </div>
    }

    override def id: String = "gr"

    override def init(parent: HTMLElement): Unit = {}
  }

  def root: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="root" class="container-fluid">
    <div class="container">
      {mainBag.xml()}
    </div>
  </div>)


}

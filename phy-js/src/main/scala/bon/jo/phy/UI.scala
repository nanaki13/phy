package bon.jo.phy

import bon.jo.html.DomShell.{$, Obs,ExtendedElement}
import bon.jo.html.{DomShell, InDom, XmlHtmlView}
import bon.jo.phy.Phy.{A, P, V}

import bon.jo.phy.view.Shape.Circle
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.KeyboardEvent

import scala.xml.Node

case class UI()(implicit uIParams: UIParams) {

  import uIParams._



  def initView()(implicit ctx: CanvasRenderingContext2D): EventContext = {
    addHtmlAndEvent
  }

  def goTo(p: P)(implicit ctx: CanvasRenderingContext2D, eventContext: EventContext): Unit = {

    sun.foreach(e => {
      val p = P(e.p.x - minViewX - (width / 2) / scale,
        e.p.y - minViewY - (height / 2) / scale)
      ctx.translate(-p.x, -p.y)
      minViewX = minViewX + p.x
      minViewY = minViewY + p.y
      eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))
    })
  }

  def getCtx2D(implicit eventContext: EventContext): CanvasRenderingContext2D = {
    val cn = InDom[Div](<div id="cnt-canvas" tabindex="0"></div>)

    org.scalajs.dom.document.body.appendChild(cn.html())

    cn.me.appendChild(canvas.html())
    implicit val ct = canvas.me.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    cn.me.addEventListener[KeyboardEvent]("keypress", hadleKeyB)
    ct
  }

  def addHtmlAndEvent(implicit ctx: CanvasRenderingContext2D): EventContext = {

     def apply[A](): DomShell.Obs[A] = Obs.once[A]()
    implicit val e : ()=>Obs[String] = apply _
    org.scalajs.dom.document.body.appendChild(root.html())
    mainBag.me.appendChild(bagName("temps * ", timeup.xml()).html())
    mainBag.me.appendChild(keepTail.html())
    mainBag.me.appendChild(bagName("vitesse planéte * ", speedUp.xml()).html())
    mainBag.me.appendChild(comeBack.html())
    mainBag.me.appendChild(partirBack.html())
    mainBag.me.appendChild(turnAroond.html())
    mainBag.me.appendChild(bagName("masse du soliel : ", masseSoleilInput.xml()).html())
    mainBag.me.appendChild(bagName("interaction:", interactionType.xml()).html())
    mainBag.me.appendChild(bagName("frotement:", frtOptionIn.xml()).html())

    val correctionBag = bagName("correction:", correctionInput.xml())
    mainBag.me.appendChild(correctionBag.html())

    var b = bagName("taille:", sizeFactorInput.xml())
    mainBag.me.appendChild(b.html())

    b = bagName("masse variation:", vMas.xml())
    mainBag.me.appendChild(b.html())
    stabilise.init(mainBag.me)
    toSun.init(mainBag.me)
    replacer.init(mainBag.me)


    val eventsHandler: EventContext = new EventContext

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
        eventsHandler.action.newValue(ActionPointDynamic[Circle](s, Purpose.Move))
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
    turnAroond.me.clkOnce().suscribe(e => {
      eventsHandler.turnAround.newValue(())
      //   centreG.p.map(_.p).foreach(turnAroundSun)
    })
    interactionType.me.clkOnce().suscribe(e => {
      switchIneraction = switchIneraction.swap
      interaction = switchIneraction._1
      interactionType.me.innerText =interaction.name
      eventsHandler.ineraction.newValue(interaction)
    })


    frtOptionIn.me.UserCanUpdate().suscribe(e => {
      eventsHandler.frotement.newValue(e.trim.toDouble)
    })


    correctionBag.me.clkOnce().suscribe(e => {
      correction = !correction
      correctionInput.me.innerText = if (correction) "Oui" else "Non"
      eventsHandler.correction.newValue(correction)
    })
    replacer.me.clkOnce().suscribe(e => {
      eventsHandler.replaceAround.newValue(())

    })
    stabilise.me.clkOnce().suscribe(e => {
      eventsHandler.stabilise.newValue(())
    })


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
        new PointDynamicColorImpl(soleilMasse, other, V(), A(), Color("#FF5F1C"), Circle(20f))
      )
      eventsHandler.action.newValue(ActionPointDynamic(sunCircle.get, Purpose.PutSun))

    })
    eventsHandler
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
        eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))

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
        eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))

      }), "d" -> (() => {
        val v = dw
        minViewX += v
        ctx.translate(-v, 0)
        eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))
      }), "q" -> (() => {
        val v = dw
        minViewX -= v
        ctx.translate(v, 0)
        eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))
      }), "z" -> (() => {
        val v = dh
        minViewY += v
        ctx.translate(0, -v)
        eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))
      }), "s" -> (() => {
        val v = dh
        minViewY -= v
        ctx.translate(0, v)
        eventContext.viewPort.newValue(ViewPort(scale, P(minViewX, minViewY), V(width * scale), V(height * scale)))
      }), "p" -> (() => {
        sizeFactor *= 1.1
        eventContext.sizeFactor.newValue(sizeFactor)
      }), "m" -> (() => {
        sizeFactor *= 0.9
        eventContext.sizeFactor.newValue(sizeFactor)
      })
    )

    keysMapProcess.get(e.key).map(e => {
      e()
      true
    }).foreach(_ => {

      ctx.save()
      ctx.setTransform(1, 0, 0, -1, 1, 1)
      ctx.fillStyle = maskColor
      val x = -2 * width
      val y = -2 * height
      val w = 4 * width
      val h = 4 * height
      ctx.fillRect(x, y, w, h)
      ctx.strokeStyle = "black"
      ctx.strokeRect(x, y, w, h)
      ctx.restore()
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

  lazy val turnAroond = InDom[Div](<div id="trn" class="btn in col">tourne</div>
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
      {switchIneraction._1.name}
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
    <div id="vMasse">
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

  def bagName(name: String, node: Node) = InDom[Div]({
    <div class="col d-inline" id={name + "-id"}>
      <span>
        {name}
      </span>{node}
    </div>
  })

  val mainBag = InDom[Div](<div id="main row"></div>)
  val root = InDom[Div](<div id="root" class="container-fluid">
    <div class="container">
      {mainBag.xml()}
    </div>
  </div>)


}

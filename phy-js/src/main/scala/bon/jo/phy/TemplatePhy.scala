package bon.jo.phy

import java.util.UUID

import bon.jo.html.DomShell.{$, ExtendedElement}
import bon.jo.html.Types.FinalComponent
import bon.jo.html.cpnt.ReadImportFile
import bon.jo.html.{InDom, XmlHtmlView}
import bon.jo.phy.EventContext._
import bon.jo.phy.Grid.GridPrams
import bon.jo.phy.ImportExport.{ExportedElement, ModelExport}
import bon.jo.phy.view.UIParams
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Canvas, Div, Select, Option => OptHtml}
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js
import scala.xml._


trait AutoId {
  val id: String = UUID.randomUUID().toString
}

trait TemplatePhy {


  val params: UIParams

  import params._

  var switchIneraction: List[Interaction] = List(Interaction.Faible, Interaction.Forte, Interaction.Ressort)
  val colorChooser = new ColorChooser(Color.Magenta)
  val selection: Grid[String] = Grid[String]("selectino-cont", Grid.withLegend, "Sélection")
  val noneChoixString = "-"
  val NoneChoix: InDom[OptHtml] with XmlHtmlView[OptHtml] = optFromStringValue(noneChoixString)
  var creationInteractionElm: Interaction = switchIneraction.head

  def optFromStringValue(i: Any): InDom[OptHtml] with XmlHtmlView[OptHtml] = InDom[OptHtml](<option value={i.toString}>
    {i.toString}
  </option>)

  @inline
  def trueFalse[A](trueValue: A, falseVale: A, boolean: Boolean): A = if (boolean) trueValue else falseVale

  def display(value: UIParams): Unit = {
    timeup.me.innerText = value.scaleTime.toString
    sizeFactorInput.me.innerText = value.sizeFactor.toString
    correctionInput.me.innerText = trueFalse("Oui", "Non", value.correction)
  }

  def setUpHtml(): Unit = {

    val crCont = Grid[String]("creation", Grid.withLegend, "Création")

    var tmpGrid = Grid("prop-creation")

    tmpGrid.cellByRaw = 2
    tmpGrid :+ bagName("masse : ", newElementMassseHtml.xml()) :+ bagName("interaction:", <div>
      {interactionType.xml()}{interactionSelectionOppose.xml()}
    </div>) :+ bagName("Color : ", colorChooser)

    crCont :+ tmpGrid

    tmpGrid = Grid("button-creation")

    tmpGrid.cellByRaw = 2
    tmpGrid :+ createPoint :+ createInteraction

    crCont :+ tmpGrid
    mainBag.cellByRaw = 1


    mainBag :+ crCont


    selection.cellByRaw = 3
    var b = bagName("Planètes :", chiocePlanete.xml())
    selection :+ b

    b = bagName("Interaction :", chioceInteraction.xml())
    selection :+ b

    b = bagName("Action :", planeteAction.xml())
    selection :+ b

    b = bagName("masse variation:", vMas.xml())
    selection :+ b


    mainBag :+ selection

    val global = Grid("Global", Grid.withLegend, "Global")
    global.cellByRaw = 2
    b = bagName("temps * ", timeup.xml())
    global :+ b

    b = bagName("vitesse planéte * ", speedUp.xml())
    global :+ b


    global :+ comeBack :+ partirBack
    global :+ bagName("frotement:", frtOptionIn.xml()) :+ bagName("correction:", correctionInput.xml())
    global :+ stabilise :+ replacer :+ save :+ importModel :+ importExample
    mainBag :+ global

    val drawProp = Grid("drwProp", Grid.withLegend, "Dessin")

    drawProp :+ keepTail :+ effacer :+ bagName("taille:", sizeFactorInput.xml())

    mainBag :+ drawProp
    // mainBag:+ bagName("masse : ", masseSoleilInput.xml())
    //  mainBag:+ bagName("interaction:", interactionType.xml())


  }

//  protected lazy val movable: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
//    <div id="dyn-1" class="position-absolute mv">
//      Je Dois bouger
//    </div>
//  })
  protected lazy val stabilise: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="applyEnergyEqual" class="col in">
      Stabilise
    </div>
  })
  lazy val canvas: XmlHtmlView[Canvas] = InDom[Canvas](<canvas style="position:absolute;top:0;z-index:0;" id="gameCanvas" width={viewPort.w.x.toString} height={viewPort.h.y.toString}></canvas>)

  protected lazy val newElementMassseHtml: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="in" class="in col">
      4000
    </div>
  })

  protected lazy val effacer: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="trn" class="in col">Effacer</div>
  )

  protected lazy val comeBack: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="cmb" class="in col">revient</div>
  )
  protected lazy val partirBack: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="push" class="in col">partir</div>
  )
  protected lazy val createPoint: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="c-p" class="in col">
      Créer planète
    </div>
  })
  protected lazy val createInteraction: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="c-i" class="in col">
      Créer interaction
    </div>
  })


  protected lazy val keepTail: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="keepTail" class="in col">tracter:
      {tracerString}
    </div>
  })
  protected lazy val speedUp: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="speeUp" class="in">
      {speedFactor}
    </div>
  })
  protected lazy val timeup: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="timeup" class="in">
      {scaleTime}
    </div>
  })
  protected lazy val correctionInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="cor" class="in">
      {if (correction) "Oui" else "Non"}
    </div>
  })
  protected lazy val sizeFactorInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="sizeFactor" class="in">
      {sizeFactor}
    </div>
  })

  protected lazy val interactionType: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="inter" class="in">
      {switchIneraction.head.name}
    </div>
  })
  protected lazy val interactionSelectionOppose: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="inter-oppose" class="in">
      {creationForceOppose.name}
    </div>
  })
  protected lazy val frtOptionIn: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="frt" class="in">0</div>
  })
  protected lazy val kRessortInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="kRessortInput" class="in">
      {kRessort}
    </div>
  })

  protected lazy val vMas: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="vMasse" class="d-inline">
      <div id="pMasse" class="in">
        +
      </div>
      <div id="mMasse" class="in">
        -
      </div>
    </div>
  })
  protected lazy val pMas: Div = $[Div]("pMasse")
  protected lazy val mMas: Div = $[Div]("mMasse")
  protected lazy val toSun: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="toSun" class="col in">
      Aller au soleil
    </div>
  })
  protected lazy val replacer: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="replacer" class="col in">
      Replacer Planete
    </div>
  })

  protected lazy val save: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="save" class="col in">
      Sauvegarder
    </div>
  })

  def initalSelPupose = List(Purpose.Delete, Purpose.Follow, Purpose.DontFollow, Purpose.Move)

  def optionHtml: List[Elem] = initalSelPupose.map(e => {
    <option value={e.toString}>
      {e.name}
    </option>

  })

  protected lazy val planeteAction: InDom[Div] with XmlHtmlView[Div] = InDom[Div](
    <div id="planeteAction">
      <select id="planeteAction-select">
        {Group(optionHtml)}
      </select>
      <div class="in" id="planeteAction-ok">Appliquer</div>
    </div>)
  protected lazy val planeteActionSubmit: Div = $[Div]("planeteAction-ok")
  protected lazy val planeteActionRef: Select = $[Select]("planeteAction-select")

  protected lazy val chiocePlanete: InDom[Div] with XmlHtmlView[Div] = doSelectHtml("planeteChoice", "pl")
  protected lazy val plneteSelection: Select = $[Select]("pl")

  protected lazy val chioceInteraction: InDom[Div] with XmlHtmlView[Div] = doSelectHtml("interaction-div-sel", "inter-sel")
  protected lazy val interactionSelection: Select = $[Select]("inter-sel")

  protected lazy val importExample: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="importExample" class="in">charger un exemple</div>)

  implicit val cv: js.Any => ExportedElement = { e =>
    ModelExport.unnaply(e.asInstanceOf[ModelExport]).get
  }

  protected val importModel = new ReadImportFile[ExportedElement]()

  private def bagName(name: String, node: Node) = {
    <div class="col d-inline" id={name + "-id"}>
      <div class="in d-inline">
        {name}
      </div>{node}
    </div>
  }

  private def bagName(name: String, node: XmlHtmlView[_]) = {
    <div class="col d-inline" id={name + "-id"}>
      <div class="in d-inline">
        {name}
      </div>{node.xml()}
    </div>
  }

  private def doSelectHtml(idDiv: String, idSelect: String) = InDom[Div](
    <div id={idDiv}>
      <select id={idSelect}>
        {NoneChoix.xml()}
      </select>
    </div>)


  trait CtxMessageView extends FinalComponent[Div] {
    private var _message: String = ""
     var close: Obs[_] = _
     var closeMessage : String = _
    def message_=(m: String): Unit = {
      _message = m
      if (isInDom) {
        $[Div](id + "-m").innerText = m
        clHtml.style.display = "inline"
      }
    }

    def message: String = _message
    def clHtml: Div = $[Div](id + "-c")
    override def xml(): Elem = <div id={id} class="in ctx-message">
      <div class="container message" >
        <span id={id + "-m"}>{_message}</span><button   id={id + "-c"} type="button" class="close" aria-label="Close">
        <span aria-hidden="true">&times;</span>
      </button>
      </div>

    </div>

    override def init(parent: HTMLElement): Unit = {

      close = clHtml.clkOnce().toMany
      close.suscribe(_ => {
        message = closeMessage
        clHtml.style.display = "none"
      })
    }

    override def id: String = "ctx-message"
  }

  object ctxMessage extends CtxMessageView

  ctxMessage.message = "Salut"
  ctxMessage.closeMessage = "--"
  type SimpleGrid = Grid[Null]

  private object mainBag extends SimpleGrid {
    override def apply(): GridPrams[Null] = GridPrams[Null](Grid.noMode, "main-gr", null)
  }

  def root: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="root">
    {mainBag.xml()}{ctxMessage.xml()}
  </div>)
  //{movable.xml()}
}





package bon.jo.phy

import bon.jo.Logger
import bon.jo.html.DomShell.$
import bon.jo.html.Types.FinalComponent
import bon.jo.html.{InDom, XmlHtmlView}
import bon.jo.phy.view.UIParams
import org.scalajs.dom.html.{Canvas, Div, Select, Option => OptHtml}
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml.{Attribute, Elem, Group, MetaData, Node, Null, PrefixedAttribute, UnprefixedAttribute}

trait TemplatePhy {
  val params: UIParams

  import params._

  val noneChoixString = "-"
  val NoneChoix: InDom[OptHtml] with XmlHtmlView[OptHtml] = optFromStringValue(noneChoixString)

  def optFromStringValue(i: Any): InDom[OptHtml] with XmlHtmlView[OptHtml] = InDom[OptHtml](<option value={i.toString}>
    {i.toString}
  </option>)

  def setUpHtml(): Unit = {

    val crCont = Grid[String]("creation", Grid.withLegend,"Création")

    var tmpGrid = Grid("prop-creation")

    tmpGrid.cellByRaw = 1
    tmpGrid :+ bagName("masse : ", masseSoleilInput.xml()) :+ bagName("interaction:", interactionType.xml())

    crCont :+ tmpGrid

    tmpGrid = Grid("button-creation")

    tmpGrid.cellByRaw = 2
    tmpGrid :+ createPoint :+ createInteraction

    crCont :+ tmpGrid
    mainBag.cellByRaw = 1


    mainBag :+ crCont

    val selection = Grid[String]("selectino-cont", Grid.withLegend,"Création")
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

    val global = Grid("Global", Grid.withLegend,"Global")
    global.cellByRaw = 2
    b = bagName("temps * ", timeup.xml())
    global :+ b

    b = bagName("vitesse planéte * ", speedUp.xml())
    global :+ b


    global :+ comeBack :+ partirBack
    global :+ bagName("frotement:", frtOptionIn.xml()) :+ bagName("correction:", correctionInput.xml())
    global :+ stabilise.xml() :+ replacer.xml()
    mainBag :+ global

    val drawProp = Grid("drwProp")

    drawProp :+ keepTail.xml() :+ effacer.xml() :+ bagName("taille:", sizeFactorInput.xml())

    mainBag :+ drawProp
    // mainBag:+ bagName("masse : ", masseSoleilInput.xml())
    //  mainBag:+ bagName("interaction:", interactionType.xml())


    mainBag


  }

  protected lazy val stabilise: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="applyEnergyEqual" class="col btn in">
      Stabilise
    </div>
  })
  protected lazy val canvas: XmlHtmlView[Canvas] = InDom[Canvas](<canvas style="position:absolute;top:0;z-index:0;" id="gameCanvas" width={width.toString} height={height.toString}></canvas>)

  protected lazy val masseSoleilInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="in" class="btn in col">
      {soleilMasse}
    </div>
  })

  protected lazy val effacer: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="trn" class="btn in col">Effacer</div>
  )

  protected lazy val comeBack: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="cmb" class="btn in col">revient</div>
  )
  protected lazy val partirBack: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="push" class="btn in col">partir</div>
  )
  protected lazy val createPoint: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="c-p" class="btn in col">
      Créer planète
    </div>
  })
  protected lazy val createInteraction: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="c-i" class="btn in col">
      Créer interaction
    </div>
  })


  protected lazy val keepTail: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="keepTail" class="btn in col">tracter:
      {tracerString}
    </div>
  })
  protected lazy val speedUp: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="speeUp" class="btn in">
      {speedFactor}
    </div>
  })
  protected lazy val timeup: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="timeup" class="btn in">
      {scleTime}
    </div>
  })
  protected lazy val correctionInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="cor" class="btn in">
      {if (correction) "Oui" else "Non"}
    </div>
  })
  protected lazy val sizeFactorInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="sizeFactor" class="btn in">
      {sizeFactor}
    </div>
  })

  protected lazy val interactionType: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="inter" class="btn in">
      {switchIneraction.head.name}
    </div>
  })
  protected lazy val frtOptionIn: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="frt" class="btn in">0</div>
  })
  protected lazy val kRessortInput: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="kRessortInput" class="btn in">
      {kRessort}
    </div>
  })

  protected lazy val vMas: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="vMasse" class="d-inline">
      <div id="pMasse" class="btn in">
        +
      </div>
      <div id="mMasse" class="btn in">
        -
      </div>
    </div>
  })
  protected lazy val pMas: Div = $[Div]("pMasse")
  protected lazy val mMas: Div = $[Div]("mMasse")
  protected lazy val toSun: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="toSun" class="col btn in">
      Aller au soleil
    </div>
  })
  protected lazy val replacer: InDom[Div] with XmlHtmlView[Div] = InDom[Div]({
    <div id="replacer" class="col btn in">
      Replacer Planete
    </div>
  })

  protected lazy val planeteAction: InDom[Div] with XmlHtmlView[Div] = InDom[Div](
    <div id="planeteAction">
      <select id="planeteAction-select">
        <option value={Purpose.Delete.toString}>Supprimer</option>
        <!--option value={Purpose.Create.toString}>{Purpose.Create.toString}</option-->
      </select>
      <div class="btn in" id="planeteAction-ok">Appliquer</div>
    </div>)
  protected lazy val planeteActionSubmit: Div = $[Div]("planeteAction-ok")
  protected lazy val planeteActionRef: Select = $[Select]("planeteAction-select")

  protected lazy val chiocePlanete: InDom[Div] with XmlHtmlView[Div] = doSelectHtml("planeteChoice", "pl")
  protected lazy val plneteSelection: Select = $[Select]("pl")

  protected lazy val chioceInteraction: InDom[Div] with XmlHtmlView[Div] = doSelectHtml("interaction-div-sel", "inter-sel")
  protected lazy val interactionSelection: Select = $[Select]("inter-sel")

  private def bagName(name: String, node: Node) = {
    <div class="col d-inline" id={name + "-id"}>
      <div class="in d-inline">
        {name}
      </div>{node}
    </div>
  }

  private def doSelectHtml(idDiv: String, idSelect: String) = InDom[Div](
    <div id={idDiv}>
      <select id={idSelect}>
        {NoneChoix.xml()}
      </select>
    </div>)


  object Grid {

    sealed trait Mode[A] {
      def apply(node: Elem, param: A): Node
    }

    object withLegend extends Mode[String] {
      override def apply(node: Elem, legend: String): Node = {
        updateAtttibute(node,"class","border").copy(child =  <span class="legend">{legend}</span> +: node.child)
      }
    }

    object noMode extends Mode[Null] {
      override def apply(node: Elem, legend: Null ): Node = node
    }

    def apply(id: String): Grid[Null] = () => GridPrams(noMode, id, null)

    def apply[A](id: String, mode: Mode[A], param: A): Grid[A] = () => GridPrams(mode, id, param)
  }

  case class GridPrams[A]( mode: Grid.Mode[A],  id: String,  modeParam: A )

  case class LegendParam(string: String)
  def appendToAttribute(n: Elem, nameAtrr : String, vale : String): Elem = {
    updateAtttibute(n,nameAtrr,(n \@ nameAtrr) + vale)
  }
  def updateAtttibute(n: Elem, nameAtrr : String , vale : String): Elem = {
    val nMeta = (n.attributes.map {

      case Null => Null
      case attribute: PrefixedAttribute => attribute
      case attribute: UnprefixedAttribute if attribute.key == nameAtrr => new UnprefixedAttribute(attribute.key, vale, attribute.next)
      case attribute: UnprefixedAttribute => attribute
      case attribute: Attribute => attribute

    }).foldLeft(Agg(None))((a, b) => {
      a :+ b
    })
    val aa = nMeta.tail match {
      case Some(value) => value
      case None => n.attributes

    }
    n.copy(attributes = aa)
  }
  case class Agg(var tail: Option[MetaData]) {

    def :+(a: MetaData): Agg = {
      if (tail != null) {
        tail = tail.map(_.copy(a))
      } else {
        tail = Some(a)
      }
      this
    }
  }
  trait Grid[A] extends FinalComponent[Div] with (() => GridPrams[A]) {

    val GridPrams(mode, id, param): GridPrams[A] = apply()



    var cellByRaw = 5
    var current = 1

    def addCell(n: Elem): Grid[A] = {
      def reworked = if (n.attribute("class").isDefined && !(n \@ "class" contains (" col")) && !(n \@ "class" contains ("col ")) && !(n \@ "class" contains ("col"))) {
        updateAtttibute(n,"class",n \@ "class"+" col")
      } else if (n.attribute("class").isEmpty) {

        n.copy(attributes = n.attributes.copy(new UnprefixedAttribute("class", "col", n.attributes.last.next)))
      } else {

        n
      }

      table.head += reworked
      current += 1
      if (current > cellByRaw) {
        current = 1
        table = ListBuffer[Node]() :: table
      }
      this
    }

    def :+(n: Elem): Grid[A] = addCell(n)

    def :+(n: Node): Grid[A] = addCell(n.asInstanceOf[Elem])

    def :+(n: XmlHtmlView[_]): Grid[A] = addCell(n.xml().asInstanceOf[Elem])

    var table: List[mutable.ListBuffer[Node]] = ListBuffer[Node]() :: Nil

    override def xml(): Node = {
      def rows: List[Elem] = for {r <- table.reverse} yield {
         <div class="row">
          {Group(r)}
        </div>
      }
      mode(<div id={id}>
        {Group(rows)}
      </div>, param)
    }


    override def init(parent: HTMLElement): Unit = {}
  }

  type SimpleGrid = Grid[Null]
  private object mainBag extends SimpleGrid {
    override def apply():GridPrams[Null] = GridPrams[Null](Grid.noMode, "main-gr",null)
  }

  def root: InDom[Div] with XmlHtmlView[Div] = InDom[Div](<div id="root">
    {mainBag.xml()}

  </div>)
}

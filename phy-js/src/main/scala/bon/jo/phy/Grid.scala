package bon.jo.phy

import bon.jo.html.Types.FinalComponent
import bon.jo.html.XmlHtmlView
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Group, Node, NodeBuffer}
import Grid.GridPrams

trait Grid[A] extends FinalComponent[Div] with (() => GridPrams[A]) {

  import XmlHelp._
  val GridPrams(mode, id, param): GridPrams[A] = apply()


  var cellByRaw = 5
  var current = 1

  def addCell(n: Elem): Grid[A] = {
    def reworked = appendTokeyOrCreate(n, "class", "col")


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
  def :+(n: NodeBuffer): Grid[A] = addCell(<div>{Group(n)}</div>)

  var table: List[mutable.ListBuffer[Node]] = ListBuffer[Node]() :: Nil

  override def xml(): Elem = {
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

object Grid {
  case class GridPrams[A](mode: Grid.Mode[A], id: String, modeParam: A)
  sealed trait Mode[A] {
    def apply(node: Elem, param: A): Elem
  }

  object withLegend extends Mode[String] {
    override def apply(node: Elem, legend: String): Elem = {
      node.copy(child = <div class="border rounded m-1 p-1">
        <span class="legend">
          {legend}
        </span>{node.child}
      </div>)
    }
  }

  object noMode extends Mode[Null] {
    override def apply(node: Elem, legend: Null): Elem = node
  }

  def apply(id: String): Grid[Null] = () => GridPrams(noMode, id, null)

  def apply[A](id: String, mode: Mode[A], param: A): Grid[A] = () => GridPrams(mode, id, param)
}
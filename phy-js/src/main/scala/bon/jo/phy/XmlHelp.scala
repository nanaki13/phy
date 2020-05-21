package bon.jo.phy

import scala.xml._

object XmlHelp{


  def appendToAttribute(n: Elem, nameAtrr: String, vale: String): Elem = {
    updateAtttibute(n, nameAtrr, (n \@ nameAtrr) + vale)
  }

  def appendTokeyOrCreate(n: Elem, nameAtrr: String, vale: String, sep: Char = ' '): Elem = {
    if (n.attribute(nameAtrr).isDefined && (n \@ nameAtrr).contains(vale)) {

      updateAtttibute(n, nameAtrr, (n \@ nameAtrr) + sep + vale)
    } else {

      if (n.attribute(nameAtrr).isDefined) {
        n.copy(attributes = n.attributes.map(e => {
          if (e.key == nameAtrr) {
            atrTail(nameAtrr, vale, e.next)
          } else {
            e
          }
        }).toList.head)
      }
      else {
        n.copy(attributes = MetaData.concatenate(n.attributes, atrTail(nameAtrr, vale, scala.xml.Null))
        )
      }
    }
  }


  def atrTail(name: String, valeur: String, n: MetaData) = new UnprefixedAttribute(name, valeur, n)

  def updateAtttibute(n: Elem, nameAtrr: String, vale: String): Elem = {
    val nMeta = n.attributes.map {

      case Null => Null
      case attribute: PrefixedAttribute if attribute.key == nameAtrr => new PrefixedAttribute(attribute.pre, attribute.key, vale, attribute.next)
      case attribute: PrefixedAttribute => attribute
      case attribute: UnprefixedAttribute if attribute.key == nameAtrr => new UnprefixedAttribute(attribute.key, vale, attribute.next);
      case attribute: UnprefixedAttribute => attribute
      case attribute: Attribute => attribute

    }.foldLeft(Agg(None))((a, b) => {
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
      if (tail.isDefined) {

        tail = tail.map(_.copy(a))
      } else {
        tail = Some(a)
      }
      this
    }
  }

}

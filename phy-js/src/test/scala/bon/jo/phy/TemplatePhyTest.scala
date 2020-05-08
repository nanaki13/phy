package bon.jo.phy
import bon.jo.phy.view.UIParams
class TemplatePhyTest extends org.scalatest.wordspec.AnyWordSpec with TemplatePhy {
  val params: UIParams =  UIParams()
  "Xml" when {
    "have attribue" should {
      "Have the atribute updated" in {
        assert(updateAtttibute(
          <xml class="toto"></xml>,
          "class","tata"
        ) ==
          <xml class="tata"></xml> )
      }
    }
  }


val xml = <input id="test" tp="file" value="Import"></input>
  "Xml test" when {
    "have attribue" should {
      "Have the atribute updated" in {
        assert(appendTokeyOrCreate(
          xml,
          "class","col"
        ) ==
          <input id="test" tp="file" value="Import" class="col"></input> )
      }
    }
  }
}

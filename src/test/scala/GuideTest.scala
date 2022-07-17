import dev.jatan.wow.guide.Logging
import dev.jatan.wow.guide.Utils.ElementExtension
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.TextNode
import org.junit.Test
import org.scalatestplus.junit.AssertionsForJUnit

class GuideTest extends AssertionsForJUnit with Logging {
  @Test
  def allChildNodes(): Unit = {
    val document = Jsoup.parse(
      """
        |<div class="steps-container">
        |  <span class="lvl-zone">1-5 Sunstrider Isle (Eversong Woods)</span>
        |  <br>
        |	 <b>
        |    <input type="checkbox" id="01a">
        |    <label for="01a">
        |      <span></span>
        |    </label>
        |    <span class="highlight"><label for="01a">01)</label></span>
        |  </b>
        |  <label for="01a"> Right in front of you, accept "
        |    <a rel="nofollow" class="accept" target="_blank" href="http://www.wowhead.com/?quest=8325">
        |      Reclaiming Sunstrider Isle
        |    </a>"
        |	   <button class="way-button" data-clipboard-text="/way Eversong Woods 38.22 20.83">38.21</button>.
        |  </label>
        |  <br>
        |</div>
        |""".stripMargin)

    val childNodes = document.select(".steps-container")
      .first()
      .allChildNodes()
    for (node <- childNodes) {
      node match {
        case t: TextNode => log.info(t.text)
        case e: Element => log.info(e.toString)
        case n => throw new RuntimeException(s"Unknown element $n")
      }
    }
  }
}

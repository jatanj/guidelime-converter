package dev.jatan.wow.guide

import com.github.openjson.{JSONArray, JSONObject}

import java.util.regex.Pattern
import scala.collection.mutable
import scala.util.{Success, Try}

object Wowhead extends Logging {
  private lazy val JsonPattern = Pattern.compile("new Listview\\((.*)\\);")

  object Template extends Enumeration {
    val Zone = Value("zone")
    val Quest = Value("quest")
  }

  object JsonFields {
    val Template = "template"
    val Data = "data"
  }

  def json(html: String): Seq[WowheadData] = {
    val result = mutable.ArrayBuffer.empty[Option[WowheadData]]

    val m = JsonPattern.matcher(html)
    while (m.find()) {
      val json = m.group(1).trim
      if (json.nonEmpty) {
        result += buildWowheadData(new JSONObject(json))
      } else {
        log.error(s"JSON is empty")
      }
    }

    result.flatten.toSeq
  }

  private def buildWowheadData(obj: JSONObject): Option[WowheadData] = {
    val template = obj.getString(JsonFields.Template)
    val data = obj.getJSONArrayOpt(JsonFields.Data)

    (template.toLowerCase.trim, data) match {
      case (template, Some(data)) =>
        Try(Template.withName(template)) match {
          case Success(t) => Some(WowheadData(t, data))
          case _ =>
            log.error(s"Failed to parse template from '$template'")
            None
        }
      case _ =>
        log.error(s"Failed to extract template and data fields from $obj")
        None
    }
  }

  case class WowheadData(
    template: Template.Value,
    data: JSONArray
  )

  implicit class JSONObjectExtension(val obj: JSONObject) extends AnyVal {
    def getStringOpt(name: String) = Try(obj.getString(name)).toOption
    def getLongOpt(name: String) = Try(obj.getLong(name)).toOption
    def getJSONArrayOpt(name: String) = Try(obj.getJSONArray(name)).toOption
  }

  implicit class JSONArrayExtension(val obj: JSONArray) extends AnyVal {
    def flatMapObject[R](f: JSONObject => Option[R]): IndexedSeq[R] = {
      val result = new mutable.ArrayBuffer[Option[R]](obj.length())
      for (i <- 0 until obj.length()) {
        result += f(obj.getJSONObject(i))
      }
      result.flatten.toIndexedSeq
    }
  }
}

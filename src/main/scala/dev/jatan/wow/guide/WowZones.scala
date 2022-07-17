package dev.jatan.wow.guide

import com.github.openjson.JSONObject
import dev.jatan.wow.guide.GuidelimeGuide.LevelRange
import dev.jatan.wow.guide.Wowhead.{JSONArrayExtension, JSONObjectExtension}

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class WowZones(version: WowVersion.Value) extends Logging {
  import WowZones._

  private lazy val WowheadZonesUri = new URI(version match {
    case WowVersion.Classic => "https://classic.wowhead.com/zones"
    case WowVersion.TBC => "https://tbc.wowhead.com/zones"
  })
  private lazy val ZoneCacheFile = WowCache.cacheFile(s"zones_${version.toString.toLowerCase}", "wowhead_zones.html")

  def fetch(): Seq[WowZone] = {
    val zones = ZoneCacheFile match {
      case Some(p) if p.toFile.exists() =>
        parseZones(Files.readString(p))
      case _ =>
        val response = HttpClient.get(WowheadZonesUri)
        response.statusCode() match {
          case 200 =>
            ZoneCacheFile.foreach(p => {
              Files.write(p, response.body().getBytes(StandardCharsets.UTF_8))
            })
            val zones = parseZones(response.body())
            zones
          case s =>
            log.error(s"Failed to fetch zones ($s) : ${response.body()}")
            Seq.empty
        }
    }

    log.info(s"Loaded ${zones.size} zones")
    zones
  }

  private def parseZones(html: String): Seq[WowZone] = {
    Wowhead.json(html)
      .find(_.template == Wowhead.Template.Zone)
      .map(_.data.flatMapObject(convertObject))
      .getOrElse(Seq.empty)
  }

  def convertObject(obj: JSONObject): Option[WowZone] = {
    for {
      name <- obj.getStringOpt("name").map(_.trim)
    } yield {
      WowZone(
        name = name,
        levelRange = parseLevelRange(obj.getLongOpt("minlevel"), obj.getLongOpt("maxlevel"))
      )
    }
  }

  def parseLevelRange(minLevel: Option[Long], maxLevel: Option[Long]) = {
    (minLevel, maxLevel) match {
      case (Some(min), Some(max)) => Some(min.toInt, max.toInt)
      case (Some(min), None) => Some(min.toInt, min.toInt)
      case (None, Some(max)) => Some(max.toInt, max.toInt)
      case _ => None
    }
  }
}

object WowZones {
  case class WowZone(
    name: String,
    levelRange: Option[LevelRange]
  )

}
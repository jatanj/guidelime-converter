package dev.jatan.wow.guide

import com.github.openjson.JSONObject
import dev.jatan.wow.guide.GuidelimeGuide.LevelRange
import dev.jatan.wow.guide.Wowhead.{JSONArrayExtension, JSONObjectExtension}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable

class WowQuests(version: WowVersion.Value) extends Logging {
  import WowQuests._

  private lazy val QuestCacheDir = WowCache.cacheDir(s"quest_${version.toString.toLowerCase}")
  private lazy val WowheadQuestUri = version match {
    case WowVersion.Classic => "https://classic.wowhead.com/quests"
    case WowVersion.TBC => "https://tbc.wowhead.com/quests"
  }

  private lazy val LevelRanges: Seq[LevelRange] =
    Seq(
      Seq(
        (1, 10),
        (11, 20),
        (21, 30),
        (31, 40),
        (41, 50),
        (51, 59),
        (60, 60),
      ),
      version match {
        case WowVersion.Classic => Seq.empty
        case WowVersion.TBC => Seq((61, 69), (70, 70))
      }
    ).flatten

  private lazy val Queries = LevelRanges.flatMap(lvl => Seq(
    WowheadQuestQuery(lvl._1, lvl._2, QuestSide.Alliance),
    WowheadQuestQuery(lvl._1, lvl._2, QuestSide.Horde),
  ))


  def fetch(): Map[Int, WowQuest] = {
    val quests = mutable.LinkedHashMap.empty[Int, WowQuest]
    def add(seq: Seq[WowQuest]): Unit = for (q <- seq) quests(q.id) = q

    for (q <- Queries) {
      questCacheFile(q) match {
        case Some(p) if p.toFile.exists() =>
          add(parseQuests(Files.readString(p)))
        case path =>
          val response = HttpClient.get(searchQuests(q))
          response.statusCode() match {
            case 200 =>
              path.foreach(p => Files.write(p, response.body().getBytes(StandardCharsets.UTF_8)))
              val quests = parseQuests(response.body())
              log.info(s"$q => ${quests.size}")
              if (quests.size >= WowheadQueryMaxResults) {
                log.error(s"Query result size (${quests.size}) exceeded max allowed (some quests will be missing)")
              }
              add(quests)
              Thread.sleep(WowheadQuerySleepMillis)
            case s =>
              log.error(s"Failed to fetch quests ($s) : ${response.body()}")
          }
      }
    }

    log.info(s"Loaded ${quests.size} quests")
    quests.toMap
  }

  private def searchQuests(q: WowheadQuestQuery) =
    s"$WowheadQuestUri/min-level:${q.minLevel}/max-level:${q.maxLevel}/side:${q.side.id}"

  private def parseQuests(html: String) = {
    Wowhead.json(html)
      .find(_.template == Wowhead.Template.Quest)
      .map(_.data.flatMapObject(convertObject))
      .getOrElse(Seq.empty)
  }

  private def convertObject(obj: JSONObject) = {
    for {
      name <- obj.getStringOpt("name")
      id <- obj.getStringOpt("id")
    } yield {
      WowQuest(name, id.toInt)
    }
  }

  private def questCacheFile(query: WowheadQuestQuery) = {
    val fileName = s"wowhead_${query.minLevel}-${query.maxLevel}_${query.side.toString.toLowerCase}.html"
    QuestCacheDir.map(d => Paths.get(d.toString, fileName))
  }
}

object WowQuests {
  private val WowheadQueryMaxResults = 1000
  private val WowheadQuerySleepMillis = 1000

  case class WowQuest(
    name: String,
    id: Int
  )

  private case class WowheadQuestQuery(
    minLevel: Int,
    maxLevel: Int,
    side: QuestSide.Value
  )

  private object QuestSide extends Enumeration {
    val Alliance = Value(1)
    val Horde = Value(2)
  }

}
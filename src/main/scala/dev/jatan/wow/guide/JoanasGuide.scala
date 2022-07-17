package dev.jatan.wow.guide

import dev.jatan.wow.guide.Utils.{ElementExtension, StringExtension}
import dev.jatan.wow.guide._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element, Node, TextNode}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.regex.{MatchResult, Pattern}
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.{Failure, Success, Try}

class JoanasGuide(category: GuideCategory) extends GuideBuilder with Logging {
  import JoanasGuide._

  private lazy val Quests = new WowQuests(category.version).fetch()
  private lazy val ZoneNames = new WowZones(category.version).fetch().map(z => (z.name, z.name.toLowerCase))

  override def build(path: Path): Option[GuidelimeGuide] = {
    val guideFiles = buildGuideFiles(path)

    val guidelimeGuide = new GuidelimeGuide(
      GuidelimeGuideDescription(
        guideName = s"Joana ${category.faction}",
        addonName = s"Guidelime_Joana_${category.faction}",
        author = "Joana",
        category = category
      ),
      guideFiles
    )

    Some(guidelimeGuide)
  }

  private def buildGuideFiles(path: Path): Seq[GuidelimeGuideFile] = {
    path.toFile.listFiles.toIndexedSeq.flatMap(f => {
      Utils.fileExtension(f.getName).toLowerCase match {
        case "html" => buildGuideFile(f)
        case _ => None
      }
    })
  }

  private def buildGuideFile(file: File): Seq[GuidelimeGuideFile] = {
    Try(Jsoup.parse(file, StandardCharsets.UTF_8.name())) match {
      case Success(doc) => buildGuideFile(file.getName, doc)
      case Failure(e) =>
        log.error(s"Failed to parse $file", e)
        Seq.empty
    }
  }

  private def buildGuideFile(fileName: String, doc: Document): Seq[GuidelimeGuideFile] = {
    val result = mutable.ArrayBuffer.empty[GuidelimeGuideFile]
    var sectionNumber = InitialSectionNumber

    var previousFile: Option[GuidelimeGuideFile] = None
    var previousDescription: Option[GuidelimeFileDescription] = None

    for (stepsContainer <- doc.select(".guide-container .steps-container").asScala) {
      val steps = mutable.ArrayBuffer.empty[Seq[Node]]
      val buffer = mutable.ArrayBuffer.empty[Node]

      val levelAndZone = Option(stepsContainer.select(s".$LevelAndZoneClassName").first()).map(_.text)

      val childNodes =
        if (levelAndZone.isEmpty) {
          stepsContainer.allChildNodes(filterChildNodes)
        } else {
          // TODO: Find a better way to do this
          stepsContainer.allChildNodes(filterChildNodes).filter(n => n match {
            case e: Element => e.className != LevelAndZoneClassName
            case t: TextNode => t.text.trim != levelAndZone.get.trim
            case _ => true
          })
        }

      for (child <- childNodes) {
        child match {
          case elem: Element if elem.nodeName == "br" =>
            if (buffer.nonEmpty) {
              steps += buffer.toIndexedSeq
              buffer.clear()
            }
          case node => buffer += node
        }
      }
      steps += buffer.toIndexedSeq

      if (previousDescription.isEmpty) {
        previousDescription = parseDescriptionFromHeader(fileName, doc)
      }
      previousDescription = previousDescription.map(_.copy(number = sectionNumber))

      buildGuideFile(fileName, levelAndZone, steps, sectionNumber, previousDescription) match {
        case Some(f) =>
          previousFile.foreach(p => p.lines += GuidelimeGuide.Code.nextGuide(f.description))
          previousFile = Some(f)
          previousDescription = Some(f.description)
          result += f
        case None =>
      }

      sectionNumber += 1
    }

    result.toSeq
  }

  private def buildGuideFile(
    fileName: String,
    levelAndZone: Option[String],
    steps: mutable.Seq[Seq[Node]],
    sectionNumber: Int,
    previous: Option[GuidelimeFileDescription]
  ): Option[GuidelimeGuideFile] = {
    buildFileDescription(fileName, levelAndZone, previous, sectionNumber) match {
      case Some(d) if d.`class`.isEmpty || d.`class` == category.`class` =>
        val lines = buildSteps(d, steps)
        Some(GuidelimeGuideFile(
          fileName = buildFileName(d),
          description = d,
          lines = lines,
        ))
      case Some(d) if d.`class`.nonEmpty && d.`class` != category.`class` =>
        log.info(s"Skipping section for class ${d.`class`.get}")
        None
      case d =>
        log.error(s"$d : Skipping section \nlevelAndZone=$levelAndZone \nsteps=${steps.map(string)} \nprevious=$previous")
        None
    }
  }

  private def buildFileName(sectionDescription: GuidelimeFileDescription) = {
    val builder = new StringBuilder()
    sectionDescription.levelRange match {case (from, to) => builder.append(s"$from-${to}_")}
    sectionDescription.`class`.foreach(cls => builder.append(s"${cls.toString}_"))
    builder.append(normalizeZoneName(sectionDescription.zone))
    builder.append('_')
    builder.append(sectionDescription.number)
    builder.append('.')
    builder.append(FileExtension)
    builder.toString
  }

  private def normalizeZoneName(zone: String) = {
    var z = zone.replaceAll("[ ,/()\\[\\]&:]", "_").replaceAll("[']", "")
    z = DoubleUnderscorePattern.matcher(z).replaceAll("_")
    if (z.startsWith("_")) {
      z = z.substring(1)
    }
    if (z.endsWith("_")) {
      z = z.substring(0, z.length - 1)
    }
    z
  }

  private def buildSteps(
    description: GuidelimeFileDescription,
    steps: mutable.Seq[Seq[Node]]
  ): mutable.Buffer[String] = {
    def isAnchor(n: Node) = Option(n) match {
      case Some(e: Element) => e.nodeName == "a"
      case _ => false
    }

    def normalizeText(text: String) = text.replaceAll("[\\[\\]]", "")

    def elementToText(node: Node): String = {
      val elem = node match {
        case n: TextNode if !isAnchor(n.parent) => return normalizeText(n.text)
        case e: Element => e
        case _ => return ""
      }

      elem.nodeName match {
        case "a" =>
          val href = elem.attr("href").toLowerCase
          val text = elem.text.trim.toLowerCase
          val m = QuestLinkPattern.matcher(href)
          if (m.matches()) {
            val questId = m.group(1).toIntOption
            if (!IgnoreQuestNames.contains(text)) {
              questId match {
                case Some(id) =>
                  Quests.get(id) match {
                    case Some(quest) =>
                      val questDbName = quest.name.trim.toLowerCase
                      if (questDbName != text) {
                        log.warn(s"Quest Mismatch : '$questDbName' != '$text'")
                      }
                    case _ =>
                      log.error(s"Quest id $id not found in DB")
                  }
                case _ =>
              }
            }
            (questId, elem.className.toLowerCase) match {
              case (Some(id), s) if s.contains("accept") => GuidelimeGuide.Code.questAccept(id)
              case (Some(id), s) if s.contains("turnin") || s.contains("turnin") => GuidelimeGuide.Code.questTurnIn(id)
              case (Some(id), s) if s.contains("skipped") => GuidelimeGuide.Code.questSkip(id)
              case (Some(id), s) if s.contains("do") => GuidelimeGuide.Code.questComplete(id)
              case (Some(_), s) if s.contains("general") => normalizeText(elem.text)
              case _ =>
                log.error(s"Failed to parse quest id for '$href' (classNames=${elem.className})")
                ""
            }
          } else if (VideoHosts.exists(href.contains)) {
            "" // TODO: Add video links to guide
          } else {
            normalizeText(elem.text)
          }
        case _ => ""
      }
    }

    val lines = mutable.ArrayBuffer.empty[String]
    val zoneName = findZoneName(description)

    for (step <- steps) {
      val stepText = new StringBuilder()
      var previousText = ""

      for (s <- step) {
        var t = elementToText(s)
        if (stepText.isEmpty) {
          t = t.trimLeft
        }
        stepText.append(t)
        previousText = t.toLowerCase
      }

      var text = stepText.toString

      zoneName match {
        case Some(zoneName) =>
          text = CoordinatesPattern.matcher(text).replaceAll((result: MatchResult) => {
            (result.group(1).toDoubleOption, result.group(2).toDoubleOption) match {
              case (Some(x), Some(y)) =>
                GuidelimeGuide.Code.goTo(x, y, Some(zoneName))
              case _ =>
                val fullMatch = result.group(0)
                log.error(s"Failed to convert match to coords: $fullMatch")
                fullMatch
            }
          })
        case None =>
          log.trace(s"Zone name not found for '${description.zone}'")
      }

      for (p <- SetHearthstonePatterns) {
        text = p.matcher(text).replaceAll((result: MatchResult) => {
          GuidelimeGuide.Code.setHearthstone(result.group())
        })
      }
      for (p <- UseHearthstonePatterns) {
        text = p.matcher(text).replaceAll((result: MatchResult) => {
          GuidelimeGuide.Code.useHearthstone(result.group())
        })
      }

      val targetClass = category.`class`.toString.toLowerCase
      text.toLowerCase.trim match {
        case t if IgnoreLinesBeginningWith.exists(t.startsWith) => // Ignore
        case t if WowClassNames.keySet.exists(v => t.startsWith(v)) && !t.startsWith(targetClass) => // Ignore
        case t if t.nonEmpty => lines += text
        case _ => // Ignore empty line
      }
    }

    lines
  }

  private def buildFileDescription(
    fileName: String,
    levelAndZone: Option[String],
    previous: Option[GuidelimeFileDescription],
    sectionNumber: Int
  ): Option[GuidelimeFileDescription] = {
    def parseLevelRange(parts: Array[String], index: Int) = {
      val levels = parts(index).split("-").flatMap(s => {
        Try(s.replaceAll("\\+", "").toInt) match {
          case Success(n) => Some(n)
          case Failure(_) => None
        }
      })
      levels match {
        case Array(from, to) => Some(from, to)
        case Array(level) => Some(level, level)
        case _ => None
      }
    }

    def parse(text: String) = {
      var wowClass: Option[WowClass.Value] = None
      val parts = text.split(" ").map(_.trim).filter(_.nonEmpty)
      if (parts.length >= 2) {
        var start = 0
        var end = parts.length
        while ({
          var continue = true
          parts(start).toLowerCase.trim.replaceAll("[\\[\\]]", "") match {
            case "level" | "levels" => start += 1
            case s if WowClassNames.contains(s) =>
              wowClass = Some(WowClassNames(s))
              start += 1
            case _ => continue = false
          }
          continue
        }) {}
        parseLevelRange(parts, start)
          .orElse({
            // Level range might also be at the end
            val range = parseLevelRange(parts, parts.length - 1)
            if (range.nonEmpty) {
              start = -1
              end = parts.length - 1
            }
            range
          })
          .orElse({
            val p = previous.map(p => {
              log.trace(s"Using previous level range ${p.levelRange} for section '$text'")
              start -= 1
              p.levelRange
            })
            if (p.isEmpty) {
              log.trace(s"Skipping section '$text' in '$fileName'")
            }
            p
          })
          .flatMap(lvl => {
            parts.slice(start + 1, end).mkString(" ").replaceAll("[\\[\\]]", "") match {
              case "Options" => previous.map(d => GuidelimeFileDescription(wowClass, lvl, d.zone, sectionNumber))
              case zone => Some(GuidelimeFileDescription(wowClass, lvl, zone, sectionNumber))
            }
          })
      } else {
        if (previous.isEmpty) {
          log.error(s"$fileName : Previous description not defined")
        }
        previous
      }
    }

    levelAndZone.flatMap(parse).orElse(previous)
  }

  private def parseDescriptionFromHeader(fileName: String, doc: Document) = {
    def parse(text: String) = {
      var t = text.replaceAll("[()]", "")
      for (f <- WowFaction.values) {
        t = t.replaceAll(f.toString, "")
      }
      buildFileDescription(fileName, Some(t), None, HeaderSectionNumber)
    }

    Option(doc.select("h1").first()) match {
      case Some(h1) => parse(h1.ownText)
      case _ => None
    }
  }

  private def filterChildNodes(node: Node): Boolean = {
    if (HiddenStylePattern.matcher(node.attr("style")).matches()) {
      return false
    }

    if (ExcludeOnClickPrefix.exists(s => node.attr("onclick").startsWith(s.toLowerCase))) {
      return false
    }

    val (elementClass, wowClasses) = node match {
      case e: Element =>
        val elementClass = e.className.toLowerCase
        (elementClass, WowClassNames.filter { case (k, _) => elementClass.contains(s"$k-display")})
      case _ => ("", Map.empty[String, WowClass.Value])
    }

    category.`class` match {
      case Some(c) => wowClasses.isEmpty || wowClasses.contains(c.toString.toLowerCase)
      case None => wowClasses.isEmpty || elementClass.contains("default-display")
    }
  }

  private def findZoneName(description: GuidelimeFileDescription): Option[String] = {
    val zone = description.zone.toLowerCase match {
      case "southern barrens" => "the barrens"
      case s if s.startsWith("barrens") => "the barrens"
      case "ruins of silvermoon" => "eversong woods"
      case "hinterlands" => "the hinterlands"
      case z => z
    }

    ZoneNames.find(s => zone.contains(s._2)).map(_._1)
  }

  private def string(nodes: Seq[Node]) = {
    nodes
      .map {
        case elem: Element => elem.text
        case n => n.toString
      }
  }
}

object JoanasGuide {
  private lazy val QuestLinkPattern = Pattern.compile(".*quest=(\\d+).*")
  private lazy val CoordinatesPattern = Pattern.compile("(\\d+)\\.(\\d+)")
  private lazy val DoubleUnderscorePattern = Pattern.compile("[_]{2,}")
  private lazy val HiddenStylePattern = Pattern.compile(".*display:[ \t]*none.*")

  private lazy val WowClassNames = WowClass.values.map(v => (v.toString.toLowerCase, v)).toMap
  private val FileExtension = "lua"

  private val LevelAndZoneClassName = "lvl-zone"
  private val ExcludeOnClickPrefix = Seq("copying_function")

  private val HeaderSectionNumber = 0
  private val InitialSectionNumber = 1

  private val IgnoreQuestNames = Seq("part", "quest", "escort quest")

  private val SetHearthstonePatterns = Seq(
    Pattern.compile("[Nn]ew [Hh]ome"),
    Pattern.compile("[Yy]our [Hh]ome")
  )
  private val UseHearthstonePatterns = Seq(
    Pattern.compile("[Hh]earth [Tt]o")
  )

  private val VideoHosts = Seq("youtube.com")

  private lazy val IgnoreLinesBeginningWith = Seq(
    "first pick the class you are playing",
    "remember to choose your class"
  )
}

package dev.jatan.wow.guide

import dev.jatan.wow.guide.GuidelimeGuide.LevelRange
import dev.jatan.wow.guide.Utils.StringBuilderExtension

import scala.collection.mutable

class GuidelimeGuide(description: GuidelimeGuideDescription, val files: Seq[GuidelimeGuideFile]) extends Guide {
  import GuidelimeGuide._

  private lazy val addonFiles = buildFiles(description, files)

  override def buildAddon(): WowAddon = {
    val toc = WowAddonToc(
      addonName = description.addonName,
      author = description.author,
      version = HardcodedVersion,
      interface = HardcodedInterface,
      dependencies = HardcodedDependencies,
      notes = description.guideName
    )
    new WowAddon(toc, addonFiles)
  }
}

object GuidelimeGuide {
  val HardcodedVersion = "1.0"
  val HardcodedInterface = "11306"
  val HardcodedDependencies = Seq("Guidelime")

  val InternalCodeStart = "@{{"
  val InternalCodeEnd = "}}@"
  val GuidelineCodeStart = "["
  val GuidelineCodeEnd = "]"

  type LevelRange = (Int, Int)

  private def buildFiles(
    description: GuidelimeGuideDescription,
    files: Seq[GuidelimeGuideFile]
  ): Seq[WowAddonFile] = {
    files.map(f => WowAddonFile(f.fileName, register(description, f)))
  }

  private def register(description: GuidelimeGuideDescription, file: GuidelimeGuideFile): String = {
    val builder = new StringBuilder()

    builder.appendLine(replaceCode(Code.zoneHeader(file.description)))
    builder.appendLine(replaceCode(Code.description(description.guideName)))
    builder.appendLine(replaceCode(Code.category(description.category)))

    for (line <- file.lines) {
      builder.appendLine(replaceCode(line))
    }

    s"""Guidelime.registerGuide([[${builder.toString}]], "${description.guideName}")"""
  }

  private def replaceCode(line: String) = {
    line.replace(InternalCodeStart, GuidelineCodeStart).replace(InternalCodeEnd, GuidelineCodeEnd)
  }

  object Code {
    def zoneHeader(description: GuidelimeFileDescription) = code(s"N ${string(description)}")

    def description(description: String) = code(s"D $description")

    def category(category: GuideCategory) = {
      val list = Seq(
        Some(category.faction.toString),
        category.`class`.map(_.toString),
        category.race.map(_.toString)
      ).flatten
      code(s"GA ${list.mkString(",")}")
    }

    def questAccept(id: Int) = code(s"QA$id")
    def questTurnIn(id: Int) = code(s"QT$id")
    def questComplete(id: Int) = code(s"QC$id")
    def questSkip(id: Int) = code(s"QS$id")

    def goTo(x: Double, y: Double, zone: Option[String]) = {
      val zoneText = zone.map(" " + _).getOrElse("")
      code(s"G $x,$y$zoneText")
    }

    def setHearthstone(text: String) = code(s"S $text")
    def useHearthstone(text: String) = code(s"H $text")

    def nextGuide(description: GuidelimeFileDescription) = code(s"NX ${string(description)}")

    private def string(description: GuidelimeFileDescription) = {
      val className = description.`class`.map(_.toString + " ").getOrElse("")
      s"${description.levelRange._1}-${description.levelRange._2} $className${description.zone} ${description.number}"
    }

    private def code(s: String) = s"$InternalCodeStart$s$InternalCodeEnd"
  }
}

case class GuidelimeGuideFile(
  fileName: String,
  description: GuidelimeFileDescription,
  lines: mutable.Buffer[String],
)

case class GuidelimeFileDescription(
  `class`: Option[WowClass.Value],
  levelRange: LevelRange,
  zone: String,
  number: Int
)

case class GuidelimeGuideDescription(
  guideName: String,
  addonName: String,
  author: String,
  category: GuideCategory
)

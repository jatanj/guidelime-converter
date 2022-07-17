package dev.jatan.wow.guide

import dev.jatan.wow.guide.JoanasGuide

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success, Try}

case class CliArgs(
  version: WowVersion.Value,
  guideType: String,
  inputPath: Path,
  outputPath: Path,
  faction: WowFaction.Value,
  cls: Option[WowClass.Value],
  race: Option[WowRace.Value]
)

object App extends Logging {
  def Usage =
    """
      |Usage:
      |  GuideConverter.jar <input> <output> [args]
      |
      |Required Arguments:
      |  --version   WoW version of the guide ('classic' or 'tbc')
      |  --type      Type of the guide (e.g. 'joana')
      |  --faction   WoW faction the guide is for ('alliance' or 'horde')
      |
      |Optional Arguments:
      |  --class     WoW class the guide is for (e.g. 'Druid')
      |  --race      WoW race the guide is for (e.g. 'Night Elf')
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    parseArgs(args.toList) match {
      case Some(cliArgs) => writeGuide(cliArgs)
      case None =>
        System.out.println(Usage)
        System.exit(1)
    }
  }

  private def writeGuide(args: CliArgs) = {
    def write(guide: Guide): Unit = guide.buildAddon().write(args.outputPath)
    def logFailure(t: String): Unit = log.error(s"Failed to build '$t' guide (args=$args)")

    val category = GuideCategory(
      version = args.version,
      faction = args.faction,
      `class` = args.cls,
      race = args.race
    )
    guideType(args.guideType).map {
      case JoanasGuide =>
        new JoanasGuide(category).build(args.inputPath) match {
          case Some(g) => write(g)
          case None => logFailure(args.guideType)
        }
      case _ =>
        log.error(s"Invalid guide type '${args.guideType}")
    }
  }

  private def guideType(t: String) = {
    t.toLowerCase.trim match {
      case "joana" | "joanas" => Some(JoanasGuide)
      case _ => None
    }
  }

  private def parseArgs(args: List[String]): Option[CliArgs] = {
    var inputPath: Option[Path] = None
    var outputPath: Option[Path] = None
    var version: Option[WowVersion.Value] = None
    var guideType: Option[String] = None
    var faction: Option[WowFaction.Value] = None
    var cls: Option[WowClass.Value] = None
    var race: Option[WowRace.Value] = None

    var list = args match {
      case input :: output :: rest =>
        inputPath = argOpt("input-path", input, Paths.get(_))
        outputPath = argOpt("output-path", output, Paths.get(_))
        rest
      case _ => List.empty
    }
    while (list.nonEmpty) {
      list match {
        case "--version" :: value :: _ => version = argOpt("version", value.toUpperCase(), WowVersion.withName)
        case "--type" :: value :: _ => guideType = Some(value)
        case "--class" :: value :: _ => cls = argOpt("class", pascalCase(value), WowClass.withName)
        case "--faction" :: value :: _ => faction = argOpt("faction", pascalCase(value), WowFaction.withName)
        case "--race" :: value :: _ => race = argOpt("race", pascalCase(value), WowRace.withName)
        case _ =>
      }
      list = list.tail
    }

    for {
      v <- version
      ip <- inputPath
      op <- outputPath
      t <- guideType
      f <- faction
    } yield {
      CliArgs(
        version = v,
        guideType = t,
        inputPath = ip,
        outputPath = op,
        faction = f,
        cls = cls,
        race = race
      )
    }
  }

  private def argOpt[T](name: String, value: String, f: String => T): Option[T] = {
    Try(f(value)) match {
      case Success(v) => Some(v)
      case Failure(_) =>
        log.error(s"Failed to parse $name value '$value'")
        None
    }
  }

  private def pascalCase(value: String) = value.toLowerCase.split(" ").map(_.capitalize).mkString(" ")
}

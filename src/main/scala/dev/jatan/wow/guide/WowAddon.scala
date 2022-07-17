package dev.jatan.wow.guide

import dev.jatan.wow.guide.Utils.StringBuilderExtension

import java.io.FileWriter
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Failure
import scala.util.Using

class WowAddon(toc: WowAddonToc, val files: Seq[WowAddonFile]) extends Logging {
  def write(path: Path): Unit = {
    val f = path.toFile
    if (!f.exists()) {
      f.mkdirs()
    }
    if (!f.isDirectory) {
      log.error(s"$f is not a directory")
      return
    }

    def newFile(name: String) = Paths.get(path.toString, name).toFile

    Using(new FileWriter(newFile(s"${toc.addonName}.toc"))) { writer =>
      writer.write(buildTocContent())
    }

    for (f <- files) {
      log.info(s"${f.fileName} : ${f.content.length} bytes")
      Using(new FileWriter(newFile(f.fileName))) { writer =>
        writer.write(f.content)
      } match {
        case Failure(e) => log.error(s"Failed to write ${f.fileName}", e)
        case _ =>
      }
    }
  }

  private def buildTocContent(): String = {
    val builder = new StringBuilder()

    def appendSection(name: String, content: String) = {
      builder.append(s"## $name: ")
      builder.appendLine(content)
    }

    appendSection("Title", toc.addonName)
    appendSection("Author", toc.author)
    appendSection("Version", toc.version)
    appendSection("Interface", toc.interface)
    appendSection("Dependencies", toc.dependencies.mkString(","))
    appendSection("Notes", toc.notes)

    builder.append('\n')

    for (f <- files.sortBy(_.fileName)) {
      builder.appendLine(f.fileName)
    }

    builder.toString
  }
}

case class WowAddonToc(
  addonName: String,
  author: String,
  version: String,
  interface: String,
  dependencies: Seq[String],
  notes: String
)

case class WowAddonFile(fileName: String, content: String)

package dev.jatan.wow.guide

import java.nio.file.{Path, Paths}

object WowCache {
  private lazy val BaseCacheDir = Paths.get("/tmp/guide-converter/")

  def cacheDir(path: String): Option[Path] = {
    val dir = Paths.get(BaseCacheDir.toString, path)
    Some(dir).filter(createParents)
  }

  def cacheFile(dir: String, name: String): Option[Path] = {
    cacheDir(dir).map(d => Paths.get(d.toString, name))
  }

  private def createParents(p: Path): Boolean = {
    val file = p.toFile
    if (file.isFile) {
      return false
    }
    if (!file.exists()) {
      file.mkdirs()
    }
    true
  }
}

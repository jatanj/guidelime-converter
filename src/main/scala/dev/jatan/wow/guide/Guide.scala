package dev.jatan.wow.guide

import java.nio.file.Path

trait Guide {
  def buildAddon(): WowAddon
}

trait GuideBuilder {
  def build(path: Path): Option[Guide]
}

case class GuideCategory(
  version: WowVersion.Value,
  faction: WowFaction.Value,
  `class`: Option[WowClass.Value],
  race: Option[WowRace.Value]
)

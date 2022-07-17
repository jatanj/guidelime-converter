package dev.jatan.wow.guide

object WowVersion extends Enumeration {
  val Classic = Value("CLASSIC")
  val TBC = Value("TBC")
}

object WowFaction extends Enumeration {
  val Alliance = Value("Alliance")
  val Horde = Value("Horde")
}

object WowRace extends Enumeration {
  val Human = Value("Human")
  val Dwarf = Value("Dwarf")
  val Gnome = Value("Gnome")
  val NightElf = Value("Night Elf")
  val Draenei = Value("Draenei")

  val Orc = Value("Orc")
  val Troll = Value("Troll")
  val Tauren = Value("Tauren")
  val Undead = Value("Undead")
  val BloodElf = Value("Blood Elf")
}

object WowClass extends Enumeration {
  val Druid = Value("Druid")
  val Hunter = Value("Hunter")
  val Mage = Value("Mage")
  val Paladin = Value("Paladin")
  val Priest = Value("Priest")
  val Rogue = Value("Rogue")
  val Shaman = Value("Shaman")
  val Warlock = Value("Warlock")
  val Warrior = Value("Warrior")
}

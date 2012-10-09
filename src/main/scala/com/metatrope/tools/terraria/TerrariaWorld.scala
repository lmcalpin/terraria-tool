package com.metatrope.tools.terraria

import java.nio.ByteOrder.LITTLE_ENDIAN
import java.nio.ByteBuffer

import scala.collection.mutable.ListBuffer

import com.metatrope.util.ByteReader
import com.metatrope.util.IO

class TerrariaWorldHeader(val buffer: ByteBuffer) extends ByteReader {
  val version = readInt
  val name = readFixedString
  val headerID = readInt
  val x = readInt
  val y = readInt
  val w = readInt
  val h = readInt
  val sizey = unsigned(readInt)
  val sizex = unsigned(readInt)
  val spawnx = readInt
  val spawny = readInt
  val surfaceLevel = readDouble
  val rockLayer = readDouble
  val temporaryTime = readDouble
  val isDayTime = readUnsignedByte
  val moonPhase = readInt
  val isBloodMoon = readUnsignedByte
  val dungeonX = readInt
  val dungeonY = readInt
  val isBoss1Dead = readUnsignedByte
  val isBoss2Dead = readUnsignedByte
  val isBoss3Dead = readUnsignedByte
  val isGoblinSaved = if (version >= 36) readUnsignedByte else false
  val isWizardSaved = if (version >= 36) readUnsignedByte else false
  val isMechanicSaved = if (version >= 36) readUnsignedByte else false
  val isGoblinArmyDefeated = if (version >= 36) readUnsignedByte else false
  val isClownDefeated = if (version >= 36) readUnsignedByte else false
  val isShadowOrbSmashed = readUnsignedByte
  val isMeteorSpawned = readUnsignedByte
  val shadowOrbSmashed = readUnsignedByte
  val altarsDestroyed = if (version >= 36) readInt else false
  val hardMode = if (version >= 36) readUnsignedByte else false
  val invasionDelay = readInt
  val invasionSize = readInt
  val invasionType = readInt
  val invasionPointX = readDouble
}

trait TerrariaTile {
  def active: Boolean
  def isImportant: Boolean
  def tileFrameX: Int
  def tileFrameY: Int
  def isLighted: Boolean
  def isWall: Boolean
  def wallType: Int
  def isLiquid: Boolean
  def liquidLevel: Int
  def isLava: Boolean
  def hasWire: Boolean
  def tileType: TileType.TileTypeValue
}

class TerrariaTileCopy(val clonedTile: TerrariaTile, val newX: Int, val newY: Int) extends TerrariaTile {
  val x = newX
  val y = newY
  val active = clonedTile.active
  val isImportant = clonedTile.isImportant
  val tileFrameX = clonedTile.tileFrameX
  val tileFrameY = clonedTile.tileFrameY
  val isLighted = clonedTile.isLighted
  val isWall = clonedTile.isWall
  val wallType = clonedTile.wallType
  val isLiquid = clonedTile.isLiquid
  val liquidLevel = clonedTile.liquidLevel
  val isLava = clonedTile.isLava
  val hasWire = clonedTile.hasWire
  val tileType = clonedTile.tileType
}

class TerrariaTileParser10(val buffer: ByteBuffer, val header: TerrariaWorldHeader, val x: Int, val y: Int) extends ByteReader with TerrariaTile {
  val active = readBoolean
  private var tileTypeVal = if (active) readUnsignedByte else TileType.Sky.code
  val isImportant = _isImportant(tileTypeVal)
  val tileFrameX = if (isImportant) readShort else 0
  val tileFrameY = if (isImportant) readShort else 0
  val isLighted = if (header.version < 36) readBoolean else false
  val isWall = readBoolean
  val wallType = if (isWall) readUnsignedByte else 0
  val isLiquid = readBoolean
  val liquidLevel = if (isLiquid) readUnsignedByte else 0
  val isLava = if (isLiquid) readBoolean else false
  val tileType = TileType.withId(tileTypeVal, TileType.Unknown)
  val hasWire = false

  override def toString(): String = {
    "Tile type " + tileTypeVal + " placed at " + tileFrameX + "x" + tileFrameY + "."
  }

  private def _isImportant(v: Int): Boolean = {
    if (!active) return false
    val important = TileType.withId(v, TileType.Unknown).important
    important(header.version)
  }
}

class TerrariaTileParser11(val buffer: ByteBuffer, val header: TerrariaWorldHeader, val x: Int, val y: Int) extends ByteReader with TerrariaTile {
  val active = readBoolean
  private var tileTypeVal = if (active) readUnsignedByte else TileType.Sky.code
  val isImportant = _isImportant(tileTypeVal)
  val tileFrameX = if (isImportant) readShort else 0
  val tileFrameY = if (isImportant) readShort else 0
  val isLighted = false
  val isWall = readBoolean
  val wallType = if (isWall) readUnsignedByte else 0
  val isLiquid = readBoolean
  val liquidLevel = if (isLiquid) readUnsignedByte else 0
  val isLava = if (isLiquid) readBoolean else false
  val hasWire = if (header.version >= 36) readBoolean else false
  val tileType = TileType.withId(tileTypeVal, TileType.Unknown)
  val rleRemaining = if (header.version >= 36) readShort else 0

  override def toString(): String = {
    "Tile type " + tileTypeVal + " placed at " + tileFrameX + "x" + tileFrameY + "."
  }

  private def _isImportant(v: Int): Boolean = {
    if (!active) return false
    val important = TileType.withId(v, TileType.Unknown).important
    important(header.version)
  }
}

object TerrariaTileParser extends IO {
  var rleRemaining: Int = 0
  var lastTile: TerrariaTile = null
  var lastX: Int = 0
  var lastY: Int = 0

  def make(buffer: ByteBuffer, header: TerrariaWorldHeader, x: Int, y: Int): TerrariaTile = {
    if (rleRemaining == 0) {
      lastX = x; lastY = y
      val parsedTile = if (header.version >= 36) {
        val parser = new TerrariaTileParser11(buffer, header, x, y)
        rleRemaining = parser.rleRemaining
        parser
      } else new TerrariaTileParser10(buffer, header, x, y)
      lastTile = parsedTile
      lastTile
    } else {
      rleRemaining -= 1
      new TerrariaTileCopy(lastTile, x, y)
    }

  }
}

class TerrariaItem(val buffer: ByteBuffer, val header:TerrariaWorldHeader) extends ByteReader {
  val count = readUnsignedByte
  val name = if (count > 0) readFixedString else ""
  val prefix = if (count > 0 && header.version >= 36) readUnsignedByte else 0
  
  override def toString(): String = {
    count + " " + name
  }
}

class TerrariaChest(val buffer: ByteBuffer, val header:TerrariaWorldHeader) extends ByteReader {
  val active = readBoolean
  val x = if (active) readInt else 0
  val y = if (active) readInt else 0
  val contents = parseContents

  def parseContents: Array[TerrariaItem] = {
    val contents = Array.ofDim[TerrariaItem](20)
    if (active) {
      for (i <- 1 to 20) {
        val item = new TerrariaItem(buffer, header)
        contents(i - 1) = item
      }
    }
    contents
  }
  override def toString(): String = {
    "chest at " + x + "x" + y + ": " + active
  }
}

class TerrariaSign(val buffer: ByteBuffer, val id: Int) extends ByteReader {
  val active = readBoolean
  val text = if (active) readFixedString else ""
  val x = if (active) readInt else 0
  val y = if (active) readInt else 0
  override def toString(): String = {
    "sign at " + x + "x" + y + ": " + active + ", text = " + text
  }
}

class TerrariaNpc(val buffer: ByteBuffer) extends ByteReader {
  val active = readBoolean
  val name = if (active) readFixedString else ""
  val posX = if (active) readFloat else 0.0f
  val posY = if (active) readFloat else 0.0f
  val homeless = if (active) readBoolean else false
  val homeX = if (active) readInt else 0
  val homeY = if (active) readInt else 0
}

class TerrariaNpcNames(val buffer: ByteBuffer, val header:TerrariaWorldHeader) extends ByteReader {
  val merchantName = if (header.version >= 36) readFixedString else ""
  val nursesName = if (header.version >= 36) readFixedString else ""
  val armsDealersName = if (header.version >= 36) readFixedString else ""
  val dryadsName = if (header.version >= 36) readFixedString else ""
  val guidesName = if (header.version >= 36) readFixedString else ""
  val clothiersName = if (header.version >= 36) readFixedString else ""
  val demolitionistsName = if (header.version >= 36) readFixedString else ""
  val tinkerersName = if (header.version >= 36) readFixedString else ""
  val wizardsName = if (header.version >= 36) readFixedString else ""
  val mechanicsName = if (header.version >= 36) readFixedString else ""
}

class TerrariaFooter(val buffer: ByteBuffer) extends ByteReader {
  val active = readBoolean
  val name = readFixedString
  val footerID = readInt
}

class TerrariaWorld(resource: String) extends IO {
  val buffer = loadFile(resource)
  buffer.order(LITTLE_ENDIAN)
  val header = new TerrariaWorldHeader(buffer)
  val tiles = parseTiles
  val chests = parseChests
  val signs = parseSigns
  val npcs = parseNpcs
  val npcNames = parseNpcNames
  val footer = new TerrariaFooter(buffer)

  private def parseTiles: Array[Array[TerrariaTile]] = {
    val sizeX = header.sizex.toInt
    val sizeY = header.sizey.toInt
    val tiles = Array.ofDim[TerrariaTile](sizeX, sizeY)
    for (x <- 0 to sizeX - 1)
      for (y <- 0 to sizeY - 1) {
        val tile = TerrariaTileParser.make(buffer, header, x, y)
        tiles(x)(y) = tile
      }
    tiles
  }

  private def parseChests: Array[TerrariaChest] = {
    val chests = Array.ofDim[TerrariaChest](1000)
    for (i <- 1 to 1000) {
      chests(i - 1) = new TerrariaChest(buffer, header)
    }
    chests
  }

  private def parseSigns: Array[TerrariaSign] = {
    val signs = Array.ofDim[TerrariaSign](1000)
    for (i <- 1 to 1000) {
      signs(i - 1) = new TerrariaSign(buffer, i)
    }
    signs
  }

  private def parseNpcs: List[TerrariaNpc] = {
    val npcs = new ListBuffer[TerrariaNpc]
    var nextNpc = true
    while (nextNpc) {
      val npc = new TerrariaNpc(buffer)
      nextNpc = npc.active
      npcs += npc
    }
    npcs.toList
  }

  private def parseNpcNames(): TerrariaNpcNames = {
    new TerrariaNpcNames(buffer, header)
  }

  def draw(fromX: Int, fromY: Int, toX: Int, toY: Int)(out:Char => Unit) = {
    println("Emitting map from " + fromX + "x" + fromY + " to " + toX + "x" + toY)
    val startX = fromX - 1
    val startY = fromY - 1
    val sizeX = toX - fromX + 1
    val sizeY = toY - fromY + 1
    println("Total size of map will be " + sizeX + "x" + sizeY)
    val map = Array.ofDim[java.lang.Character](sizeY, sizeX)
    for (y <- 0 to sizeY - 1) {
      for (x <- 0 to sizeX - 1) {
        val tile = tiles(x + startX)(y + startY)
        val c = tile.tileType match {
          case TileType.Unknown => println("Unknown tile at loc: " + (x + startX) + "x" + (y + startY)); '?'
          case TileType.Sky => if (tile.isLiquid) 'w' else ' '
          case TileType.Door | TileType.DoorOpen => 'd'
          case TileType.Amethyst |
            TileType.Sapphire |
            TileType.Ruby |
            TileType.Diamond |
            TileType.Emerald |
            TileType.Topaz => '^'
          case TileType.Ebonstone => 'E'
          case TileType.Heart => '@'
          case TileType.Chest => '$'
          case TileType.Pot => 'P'
          case TileType.Trees => 'T'
          case TileType.Altar => 'A'
          case TileType.ShadowOrb => 'O'
          // ores
          case TileType.Iron => 'I'
          case TileType.Copper => 'C'
          case TileType.Gold => 'G'
          case TileType.Silver => 'S'
          case TileType.Demonite => 'D'
          case TileType.Hellstone => 'H'
          // ground tiles
          case TileType.DecorativePot => 'u'
          case TileType.HerbBlooming |
            TileType.HerbImmature |
            TileType.HerbMature => 'h'
          case TileType.Clay => 'c'
          case TileType.Stone => '='
          case TileType.Grass |
            TileType.CorruptionGrass => ':'
          case TileType.Mud => 'm'
          case TileType.Sand => 's'
          case TileType.Dirt => '.'
          case TileType.Plants |
            TileType.Plants2 |
            TileType.Plants3 |
            TileType.CorruptionPlants => 'p'
          case TileType.Vines |
            TileType.CorruptionVines => ';'
          // player placed tiles
          case TileType.Torch |
            TileType.Lamppost |
            TileType.Tikitorch => '`'
          case TileType.Anvil |
            TileType.Sawmill |
            TileType.Furnace |
            TileType.CraftingTable => '+'
          case TileType.Table |
            TileType.Bed |
            TileType.Bench |
            TileType.Bathtub |
            TileType.Dresser => 'f'
          case TileType.WoodenPlatform => '-'
          case TileType.BlockBlueStone |
            TileType.BlockCopper |
            TileType.BlockGold |
            TileType.BlockGreenStone |
            TileType.BlockHellstone |
            TileType.BlockObsidian |
            TileType.BlockPinkStone |
            TileType.BlockRedStone |
            TileType.BlockSilver |
            TileType.BlockStone |
            TileType.BlockWood => '|'
          // extra
          case _ => 'X'
        }
        map(y)(x) = c
      }
    }
    map.foreach { row =>
      row.foreach { char =>
        out(char)
      }
      out('\n')
    }
  }
}

object TerrariaWorld extends IO {
  private def argval(arg: String, argname: String): Option[String] = {
    if (arg.startsWith(argname))
      Some(arg.substring(argname.length()))
    else
      None
  }
  def main(args: Array[String]) {
    if (args.length == 0) {
      println("Usage: sbt run <path-to-terraria-world-file>")
      return
    }

    val path = args(0)
    val action = if (args.length >= 2) args(1) else "stats"

    val world = new TerrariaWorld(path)
    action match {
      case "map" => {
        emitMap(args, world)
      }
      case "stats" => {
        printStats(world)
      }
      case _ => println("Invalid action: " + action)
    }
  }

  private def emitMap(args: Array[String], world: com.metatrope.tools.terraria.TerrariaWorld): Unit = {
    var fromx = 1
    var fromy = 1
    var tox = world.header.sizex
    var toy = world.header.sizey
    var outfile = "map.txt"
    args.foreach { arg =>
      argval(arg, "--from=").map(x => {
        val rem = x.split("x")
        fromx = rem(0).toInt - 1
        fromy = rem(1).toInt - 1
      })
      argval(arg, "--to=").map(x => {
        val rem = x.split("x")
        tox = rem(0).toInt - 1
        toy = rem(1).toInt - 1
      })
      argval(arg, "--outfile=").map(outfile = _)
    }
    using(new java.io.FileWriter(outfile)) { fw =>
      using(new java.io.PrintWriter(fw)) { pw =>
        world.draw(fromx, fromy, tox.toInt, toy.toInt) { c:Char =>
          pw.append(c)
        }
      }
    }
  }

  private def printStats(world: com.metatrope.tools.terraria.TerrariaWorld): Unit = {
    println("Loaded world: " + world.header.name)
    if (!world.footer.name.equalsIgnoreCase(world.header.name)) {
      println("WARNING: the world file was not parsed correctly!!!")
      println("It is either corrupt or is a version too new for this parser to handle")
    }
    println("Size is: " + world.header.sizex + "x" + world.header.sizey)
    println("Spawn point is: " + world.header.spawnx + "x" + world.header.spawny)
    println("Dungeon:  " + world.header.dungeonX + "x" + world.header.dungeonY)
    println("Chests:")
    world.chests.foreach { chest =>
      if (chest.active) {
        println("Chest at " + chest.x + "x" + chest.y + " containing: " + chest.contents.deep.mkString(","))
      }
    }
    println("Signs:")
    world.signs.foreach { sign =>
      if (sign.active && sign.text.length() > 0) {
        println("Sign #" + sign.id + ": " + sign.text)
      }
    }
    println("NPCs:")
    world.npcs.foreach { npc =>
      if (npc.active) {
        println("NPC '" + npc.name + "'")
      }
    }
  }
}
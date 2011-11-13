package com.metatrope.tools.terraria

import java.io.File
import java.io.FileInputStream
import java.nio.channels.FileChannel.MapMode._
import java.nio.ByteOrder._
import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer
import java.io.InputStream
import java.nio.channels.Channels
import java.nio.channels.Channel
import java.io.ByteArrayOutputStream
import scala.util.control.Breaks._
import com.metatrope.util.IO

trait ByteReader {
    def buffer: ByteBuffer
    
    def readFixedString: String = {
        val nameLen = buffer.get
        readString(nameLen)
    }

    def readString(len: Int): String = {
        val sb = new StringBuilder
        for (i <- 1 to len) {
            sb.append(buffer.get.toChar)
        }
        sb.toString
    }

    def readByte: Byte = {
        buffer.get
    }
    
    def readInt: Int = {
        buffer.getInt
    }
    
    def readShort: Short = {
        buffer.getShort
    }
    
    def readFloat: Float = {
        buffer.getFloat
    }
    
    def readDouble: Double = {
        buffer.getDouble
    }
    
    def readBoolean: Boolean = {
        val byte = buffer.get
        return (byte != 0)
    }

    def unsigned(int: Int): Long = {
        return int & 0xffffffffL
    }
}

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
    val isDayTime = readByte
    val moonPhase = readInt
    val isBloodMoon = readByte
    val dungeonX = readInt
    val dungeonY = readInt
    val isBoss1Dead = readByte
    val isBoss2Dead = readByte
    val isBoss3Dead = readByte
    val isShadowOrbSmashed = readByte
    val shadowOrbSmashed = readByte
    val invasionDelay = readInt
    val invasionSize = readInt
    val invasionType = readInt
    val invasionPointX = readDouble
}

class TerrariaTile(val buffer: ByteBuffer, val x: Int, val y: Int) extends ByteReader {
    val active = readBoolean
    private var tileTypeVal = if (active) readByte else TileType.Sky.code
    val isImportant = _isImportant(tileTypeVal)
    val tileFrameX = if (isImportant) readShort else 0
    val tileFrameY = if (isImportant) readShort else 0
    val isLighted = readBoolean
    val isWall = readBoolean
    val wallType = if (isWall) readByte else 0
    val isLiquid = readBoolean
    val liquidLevel = if (isLiquid) readByte else 0
    val isLava = if (isLiquid) readBoolean else false
    val tileType = TileType.withId(tileTypeVal, TileType.Unknown)

    override def toString(): String = {
        "Tile type " + tileTypeVal + " placed at " + tileFrameX + "x" + tileFrameY + "."
    }

    private def _isImportant(v: Int): Boolean = {
        if (!active) return false
        val important = TileType.withId(v, TileType.Unknown).important
        important
    }
}

class TerrariaItem(val buffer: ByteBuffer) extends ByteReader {
    val count = readByte
    val name = if (count > 0) readFixedString else ""
    override def toString(): String = {
        count + " " + name
    }
}

class TerrariaChest(val buffer: ByteBuffer) extends ByteReader {
    val active = readBoolean
    val x = if (active) readInt else 0
    val y = if (active) readInt else 0
    val contents = parseContents

    def parseContents: Array[TerrariaItem] = {
        val contents = Array.ofDim[TerrariaItem](20)
        if (active) {
            for (i <- 1 to 20) {
                val item = new TerrariaItem(buffer)
                contents(i - 1) = item
            }
        }
        contents
    }
}

class TerrariaSign(val buffer: ByteBuffer, val id: Int) extends ByteReader {
    val active = readBoolean
    val text = if (active) readFixedString else ""
    val x = if (active) readInt else 0
    val y = if (active) readInt else 0
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
    val footer = new TerrariaFooter(buffer)

    private def loadFile(resource: String): ByteBuffer = {
        val file = new File(resource)
        if (file.exists()) {
            val fileSize = file.length
            val stream = new FileInputStream(file)
            return stream.getChannel.map(READ_ONLY, 0, fileSize)
        } else {
            val BUFSIZE = 4096
            val stream = getClass.getResourceAsStream("/" + resource)
            if (stream == null) {
              println("File not found: " + resource)
              exit()
            }
            val out = new ByteArrayOutputStream(BUFSIZE);
            val tmp = Array.ofDim[Byte](BUFSIZE);
            var continue = true
            while (continue) {
                val r = stream.read(tmp);
                if (r == -1)
                    continue = false
                else
                    out.write(tmp, 0, r);
            }
            return ByteBuffer.wrap(out.toByteArray());
        }
    }

    private def parseTiles: Array[Array[TerrariaTile]] = {
        val sizeX = header.sizex.toInt
        val sizeY = header.sizey.toInt
        val tiles = Array.ofDim[TerrariaTile](sizeX, sizeY)
        for (x <- 0 to sizeX - 1)
            for (y <- 0 to sizeY - 1) {
                val tile = new TerrariaTile(buffer, x, y)
                tiles(x)(y) = tile
            }
        tiles
    }

    private def parseChests: Array[TerrariaChest] = {
        val chests = Array.ofDim[TerrariaChest](1000)
        for (i <- 1 to 1000) {
            chests(i - 1) = new TerrariaChest(buffer)
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

    def emitMap(fromX:Int,fromY:Int,toX:Int,toY:Int,outfile:String) = {
        println("Emitting map from " + fromX + "x" + fromY + " to " + toX + "x" + toY)
        val startX = fromX - 1
        val startY = fromY - 1
        val sizeX = toX - fromX + 1
        val sizeY = toY - fromY + 1
        println("Total size of map will be " + sizeX + "x" + sizeY)
        val map = Array.ofDim[Character](sizeY,sizeX)
        for (y <- 0 to sizeY-1 ) {
            for (x <- 0 to sizeX-1) {
                val tile = tiles(x+startX)(y+startY)
                val c = tile.tileType match {
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
                    case TileType.Torch => '`'
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
        val sb = new StringBuilder
        printToFile(outfile) { p =>
            map.foreach { row =>
                row.foreach { char =>
                    p.append(char)
                }
                p.append("\n")
            }
        }
    }
}

object TerrariaWorld {
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
            var fromx = 1
            var fromy = 1
            var tox = world.header.sizex
            var toy = world.header.sizey
            var outfile = "map.txt"
            args.foreach { arg =>
              if (arg.startsWith("--from=")) {
                val rem = arg.substring("--from=".length()).split("x")
                fromx = rem(0).toInt - 1
                fromy = rem(1).toInt - 1
              } else if (arg.startsWith("--to=")) {
                val rem = arg.substring("--to=".length()).split("x")
                tox = rem(0).toInt - 1
                toy = rem(1).toInt - 1
              } else if (arg.startsWith("--outfile=")) {
                outfile = arg.substring("--outfile=".length())
              }
            }
            world.emitMap(fromx,fromy,tox.toInt,toy.toInt,outfile)
          }
          case "stats" => {
            println("Loaded world: " + world.header.name)
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
          case _ => println("Invalid action: " + action) 
        }
    }
}
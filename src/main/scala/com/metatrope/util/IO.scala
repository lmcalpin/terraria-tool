package com.metatrope.util
import java.io.FileWriter
import java.nio.ByteBuffer

trait IO {
  def printToFile(fileName: String)(op: java.io.PrintWriter => Unit) {
    using(new java.io.FileWriter(fileName)) { fw =>
      using(new java.io.PrintWriter(fw)) { pw =>
        op(pw)
      }
    }
  }

  def using[A, B <: { def close(): Unit }](closeme: B)(f: B => A): A = {
    try {
      f(closeme)
    } finally {
      closeme.close()
    }
  }

}

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
  
  // Byte is signed, but the Terraria file stores unsigned bytes
  def readUnsignedByte: Short = {
    (buffer.get & 0xff).toShort
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
    buffer.get != 0
  }

  def unsigned(int: Int): Long = {
    int & 0xffffffffL
  }
}


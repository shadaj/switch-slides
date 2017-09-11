package me.shadaj.jinput.leobodnar

import com.lynbrookrobotics.funkydashboard.{FunkyDashboard, TimeSeriesNumeric}
import org.hid4java.HidManager

object HidHack extends App {
  val hidServices = HidManager.getHidServices
  val joyConHid = hidServices.getHidDevice(0x57e, 0x2007, null)

  println("Connected to Joy-Con")

  val globalSubcommandIterator = Iterator.from(0).map(x => (x & 0xF).toByte)

  def memcpy[T](from: Array[T], to: Array[T], startPos: Int) = {
    from.zipWithIndex.foreach { case (v, i) =>
      to(i + startPos) = v
    }
  }

  def sendCommand(command: Byte, data: Array[Byte]): Array[Byte] = {
    val message = new Array[Byte](64)
    message(0) = command
    memcpy(data, message, 1)

    joyConHid.write(message, 64, 0)
    val into = new Array[Byte](64)
    joyConHid.read(into)

    into
  }

  def sendSubcommand(command: Byte, subcommand: Byte, data: Array[Byte]): Array[Byte] = {
    val commandData = (Array(globalSubcommandIterator.next(), 0x00.toByte, 0x01.toByte, 0x40.toByte, 0x40.toByte, 0x00.toByte, 0x01.toByte, 0x40.toByte, 0x40.toByte) :+ subcommand) ++ data
    sendCommand(command, commandData)
  }

  sendSubcommand(0x1, 0x48, Array(0x01.toByte)) // enable vibration
  sendSubcommand(0x1, 0x40, Array(0x01.toByte)) // enable IMU
  sendSubcommand(0x1, 0x3, Array(0x31.toByte)) // increase data rate

  sealed trait Button
  case object Y extends Button
  case object X extends Button
  case object B extends Button
  case object A extends Button
  case object SL extends Button
  case object SR extends Button
  case object R extends Button
  case object ZR extends Button

  def buttonStatus(result: Array[Byte]): List[Button] = {
    val byte2Buttons = List(
      0x01 -> Y,
      0x02 -> X,
      0x04 -> B,
      0x08 -> A,
      0x10 -> SR,
      0x20 -> SL,
      0x40 -> R,
      0x80 -> ZR
    )

    def getButtonsForByte(byte: Byte, mapping: List[(Int, Button)]): List[Button] = {
      mapping.filter { case (mask, button) =>
        (byte & mask) != 0
      }.map(_._2)
    }

    getButtonsForByte(result(2), byte2Buttons)
  }

  var lastRoll = 0L
  var lastPitch = 0L
  var lastYaw = 0L
  val dashboard = new FunkyDashboard(20, 8080)
  dashboard.datasetGroup("Gyro").addDataset(new TimeSeriesNumeric("Relative Roll")(lastRoll.toDouble))
  dashboard.datasetGroup("Gyro").addDataset(new TimeSeriesNumeric("Relative Pitch")(lastPitch.toDouble))
  dashboard.datasetGroup("Gyro").addDataset(new TimeSeriesNumeric("Relative Yaw")(lastYaw.toDouble))
  dashboard.start()

  def readGyroAccelPair(byte1: Byte, byte2: Byte): Long = {
    val a = (byte1.toLong << 8) | byte2
    val b = 0xFFFF.toLong - a

    if (a < b) {
      a
    } else -b
  }

  while (true) {
    val result = sendSubcommand(0x1F, 0x0, Array()).tail // ignore packet ID

    println(s"Pressed Buttons: ${buttonStatus(result).mkString(" ")}")

    val gyroData = result.toList.slice(12, 39)
    if (gyroData.head != 0) {
      lastRoll = readGyroAccelPair(gyroData(7), gyroData(8))
      lastPitch = readGyroAccelPair(gyroData(9), gyroData(10))
      lastYaw = readGyroAccelPair(gyroData(11), gyroData(12))
    }
  }
}

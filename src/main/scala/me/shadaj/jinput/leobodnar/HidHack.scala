package me.shadaj.jinput.leobodnar

import java.nio.{ByteBuffer, ByteOrder}

import com.lynbrookrobotics.potassium.streams.Stream
import com.lynbrookrobotics.funkydashboard.{FunkyDashboard, TimeSeriesNumeric}
import com.lynbrookrobotics.potassium.clock.JavaClock
import com.lynbrookrobotics.potassium.sensors.imu.DigitalGyro
import com.lynbrookrobotics.potassium.units.Value3D
import org.hid4java.HidManager
import squants.motion.{AngularVelocity, DegreesPerSecond}
import squants.time.Milliseconds

import java.awt.Robot
import java.awt.event.{InputEvent, KeyEvent}

object HidHack extends App {
  val robot = new Robot()

  implicit val clock = JavaClock
  val hidServices = HidManager.getHidServices
  val joyConHid = hidServices.getHidDevice(0x57e, 0x2007, null)

  println("Connected to Joy-Con")

  val globalSubcommandIterator = Iterator.from(0).map(x => (x & 0xF).toByte)

  def memcpy[T](from: Seq[T], to: Array[T], startPos: Int) = {
    from.zipWithIndex.foreach { case (v, i) =>
      to(i + startPos) = v
    }
  }

  def sendCommand(command: Byte, data: Seq[Byte]): Array[Byte] = {
    val message = new Array[Byte](64)
    message(0) = command
    memcpy(data, message, 1)

    joyConHid.write(message, 64, 0)
    val into = new Array[Byte](64)
    joyConHid.read(into)

    into
  }

  def readData() = {
    val into = new Array[Byte](64)
    joyConHid.read(into)

    into
  }

  def sendSubcommand(command: Byte, subcommand: Byte, data: Seq[Byte]): Array[Byte] = {
    val commandData = (Seq(globalSubcommandIterator.next(), 0x00.toByte, 0x01.toByte, 0x40.toByte, 0x40.toByte, 0x00.toByte, 0x01.toByte, 0x40.toByte, 0x40.toByte) :+ subcommand) ++ data
    sendCommand(command, commandData)
  }

  // sendSubcommand(0x1, 0x48, Seq(0x01)) // enable vibration
  // sendSubcommand(0x1, 0x40, Seq(0x01)) // enable IMU
  sendSubcommand(0x1, 0x3, Seq(0x3F)) // set to only push on button press
  sendSubcommand(0x1, 0x30, Seq(1)) // set player light to 1

  sealed trait Button
  case object Y extends Button
  case object X extends Button
  case object B extends Button
  case object A extends Button
  case object SL extends Button
  case object SR extends Button
  case object R extends Button
  case object ZR extends Button

  def buttonStatus(buttonByte: Byte): List[Button] = {
    val byte2Buttons = List(
      0x01 -> A,
      0x02 -> X,
      0x04 -> B,
      0x08 -> Y
    )

    def getButtonsForByte(byte: Byte, mapping: List[(Int, Button)]): List[Button] = {
      mapping.filter { case (mask, button) =>
        (byte & mask) != 0
      }.map(_._2)
    }

    getButtonsForByte(buttonByte, byte2Buttons)
  }

  val (buttonData, pushData) = Stream.manual[List[Button]]
  
  var stop = false
  val thread = new Thread(() => {
    while (!stop) {
      val packetID +: result = readData().toSeq // ignore packet ID
      pushData(buttonStatus(result.head))
    }
  })

  thread.start()

  buttonData.eventWhen(_.contains(A)).onStart.foreach(() => {
    robot.keyPress(KeyEvent.VK_RIGHT)
    robot.keyRelease(KeyEvent.VK_RIGHT)
  })

  buttonData.eventWhen(_.contains(B)).onStart.foreach(() => {
    robot.keyPress(KeyEvent.VK_LEFT)
    robot.keyRelease(KeyEvent.VK_LEFT)
  })

  readLine()
  stop = true
}

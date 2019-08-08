package me.shadaj.jinput.leobodnar

import java.nio.{ByteBuffer, ByteOrder}

import com.lynbrookrobotics.potassium.streams.Stream
import com.lynbrookrobotics.funkydashboard.{FunkyDashboard, TimeSeriesNumeric}
import com.lynbrookrobotics.potassium.clock.JavaClock
import com.lynbrookrobotics.potassium.sensors.imu.DigitalGyro
import com.lynbrookrobotics.potassium.units.Value3D
import org.hid4java.{HidDevice, HidManager}
import squants.motion.{AngularVelocity, DegreesPerSecond}
import squants.time.Milliseconds
import java.awt.Robot
import java.awt.event.{InputEvent, KeyEvent}
import java.io.{File, FileWriter}

import com.lynbrookrobotics.potassium.tasks.ContinuousTask

sealed abstract class Button(val bits: Int)
object Button {
  case object Y extends Button(0x01)
  case object B extends Button(0x02)
  case object A extends Button(0x04)
  case object X extends Button(0x08)

  case object L extends Button(0x10)
  case object R extends Button(0x20)
  case object ZL extends Button(0x40)
  case object ZR extends Button(0x80)

  case object SL extends Button(0x40)
  case object SR extends Button(0x80)

  case object Select extends Button(0x100)
  case object Start extends Button(0x200)
}

object DolphinGame extends App {
  val robot = new Robot()

  implicit val clock = JavaClock
  val hidServices = HidManager.getHidServices

  case class JoystickState(x: Double, y: Double)

  val lxRange = (550, 3150)
  val lyRange = (1115, 3310)
  val rxRange = (745, 3470)
  val ryRange = (645, 2970)

  def parseJoystickState(bytes: Seq[Byte], xRange: (Int, Int), yRange: (Int, Int)): JoystickState = {
    val transformedBytes = bytes.map(_.toInt).map { b =>
      if (b < 0) b + 256 else b
    }

    def inRange(value: Int, low: Int, high: Int) = {
      ((value - low).toDouble / (high - low)) * 2 - 1
    }

    JoystickState(
      inRange(transformedBytes(0) | ((transformedBytes(1) & 0xF) << 8), xRange._1, xRange._2),
      inRange((transformedBytes(1) >> 4) | (transformedBytes(2) << 4), yRange._1, yRange._2)
    )
  }

  def getButtonsForByte(byte: Byte, mapping: List[(Int, Button)]): List[Button] = {
    mapping.filter { case (mask, button) =>
      (byte & mask) != 0
    }.map(_._2)
  }

  def getJoyConStream(joyConHid: HidDevice, i: Int) = {

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

    val playerNum = i + 1

    // sendSubcommand(0x1, 0x48, Seq(0x01)) // enable vibration
    // sendSubcommand(0x1, 0x40, Seq(0x01)) // enable IMU
    sendSubcommand(0x1, 0x3, Seq(0x30)) // set to standard full mode
    sendSubcommand(0x1, 0x30, Seq(playerNum.toByte)) // set player light to 1

    val isRight = joyConHid.getProduct.endsWith("(R)")

    val (buttonData, pushData) = Stream.manual[(List[Button], JoystickState)]

    var stop = false
    val thread = new Thread(() => {
      while (!stop) {
        val result = readData().toSeq // ignore packet ID

        val origJoy = parseJoystickState(
          if (isRight) result.slice(9, 12) else result.slice(6, 9),
          if (isRight) rxRange else lxRange,
          if (isRight) ryRange else lyRange
        )

        pushData((
          if (isRight) getButtonsForByte(
            result(3),
            List(
              0x01 -> Button.Y,
              0x02 -> Button.A,
              0x04 -> Button.X,
              0x08 -> Button.B,
              0x10 -> Button.SR,
              0x20 -> Button.SL
            )
          ) else getButtonsForByte(
            result(5),
            List(
              0x01 -> Button.A,
              0x02 -> Button.X,
              0x04 -> Button.Y,
              0x08 -> Button.B,
              0x10 -> Button.SR,
              0x20 -> Button.SL
            )
          )) ++ getButtonsForByte(
            result(4),
            List(
              0x01 -> Button.Start,
              0x02 -> Button.Start
            )
          ),
          if (isRight) {
            JoystickState(origJoy.y, -origJoy.x)
          } else {
            JoystickState(-origJoy.y, origJoy.x)
          }
        )
      }
    })

    thread.start()

    (buttonData, () => { stop = true })
  }

  println(hidServices.getAttachedHidDevices.toArray.toList.asInstanceOf[List[HidDevice]])
  hidServices.getAttachedHidDevices.toArray.toList.asInstanceOf[List[HidDevice]].filter(_.getVendorId == 0x57e).zipWithIndex.foreach { case (d, i) =>
    d.open()
    val outputPipeFile = new File(System.getProperty("user.home") + s"/Library/Application Support/Dolphin/Pipes/ctrl${i + 1}")
    val outputPipeWriter = new FileWriter(outputPipeFile)

    println(d.getProduct)
    val (buttonData, _) = getJoyConStream(d, i)
    buttonData.foreach { case (_, joy) =>
      outputPipeWriter.write(f"SET MAIN ${(joy.x + 1)/2}%.2f ${(joy.y + 1)/2}%.2f\n")
      outputPipeWriter.flush()
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.A))
      e.onStart.foreach(() => {
        outputPipeWriter.write("PRESS A\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE A\n")
        outputPipeWriter.flush()
      })
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.B))
      e.onStart.foreach(() => {
        outputPipeWriter.write("PRESS B\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE B\n")
        outputPipeWriter.flush()
      })
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.SL))
      e.onStart.foreach(() => {
        outputPipeWriter.write("PRESS L\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE L\n")
        outputPipeWriter.flush()
      })
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.Y))
      e.onStart.foreach(() => {
        outputPipeWriter.write("PRESS Z\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE Z\n")
        outputPipeWriter.flush()
      })
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.X))
      e.onStart.foreach(() => {
        outputPipeWriter.write("PRESS X\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE X\n")
        outputPipeWriter.flush()
      })
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.SR))
      e.onStart.foreach(() => {
        outputPipeWriter.write("PRESS R\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE R\n")
        outputPipeWriter.flush()
      })
    }

    {
      val e = buttonData.eventWhen(_._1.contains(Button.Start))
      e.onStart.foreach(() => {
        println("start")
        outputPipeWriter.write("PRESS START\n")
        outputPipeWriter.flush()
      })

      e.onEnd.foreach(() => {
        outputPipeWriter.write("RELEASE START\n")
        outputPipeWriter.flush()
      })
    }
  }


  readLine()
  System.exit(0)
}

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

object HidHack extends App {
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

  def sendSubcommand(command: Byte, subcommand: Byte, data: Seq[Byte]): Array[Byte] = {
    val commandData = (Seq(globalSubcommandIterator.next(), 0x00.toByte, 0x01.toByte, 0x40.toByte, 0x40.toByte, 0x00.toByte, 0x01.toByte, 0x40.toByte, 0x40.toByte) :+ subcommand) ++ data
    sendCommand(command, commandData)
  }

  sendSubcommand(0x1, 0x48, Seq(0x01)) // enable vibration
  sendSubcommand(0x1, 0x40, Seq(0x01)) // enable IMU
  sendSubcommand(0x1, 0x3, Seq(0x30)) // set to standard full mode
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

  def buttonStatus(result: Seq[Byte]): List[Button] = {
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

  var lastRollRaw: Short = 0
  var lastPitchRaw: Short = 0
  var lastYawRaw: Short = 0

  val gyro = new DigitalGyro(Milliseconds(10)) {
    override def retrieveVelocity: Value3D[AngularVelocity] = {
      Value3D(
        DegreesPerSecond(lastRollRaw * (4588D / 65535)),
        DegreesPerSecond(lastPitchRaw * (4588D / 65535)),
        DegreesPerSecond(lastYawRaw * (4588D / 65535))
      )
    }
  }

  def readGyroAccelPair(byte1: Byte, byte2: Byte): Short = {
    ByteBuffer.wrap(Array(byte1, byte2)).order(ByteOrder.LITTLE_ENDIAN).getShort()
  }

  val gyroData = Stream.periodic(Milliseconds(10)) {
    val packetID +: result = sendSubcommand(0x1F.toByte, 0x0, Seq()).toSeq // ignore packet ID

    lastRollRaw = readGyroAccelPair(result(18), result(19))
    lastPitchRaw = readGyroAccelPair(result(20), result(21))
    lastYawRaw = readGyroAccelPair(result(22), result(23))

    gyro.getVelocities
  }


  Thread.sleep(5000)
  gyro.endCalibration()

  implicit class ToTimeSeriesNumeric[T](val stream: Stream[T]) extends AnyVal {
    def toTimeSeriesNumeric(name: String)(implicit ev: T => Double): TimeSeriesNumeric = {
      var lastValue: Double = 0.0
      new TimeSeriesNumeric(name)(lastValue) {
        stream.foreach { v =>
          lastValue = v
        }
      }
    }
  }

  val dashboard = new FunkyDashboard(50, 8080)
  val integrated = gyroData.map(_.x).integral.zip(gyroData.map(_.y).integral).zip(gyroData.map(_.z).integral).map { case ((roll, pitch), yaw) =>
    Value3D(roll, pitch, yaw)
  }
  dashboard.datasetGroup("Gyro").addDataset(integrated.map(_.x.toDegrees).toTimeSeriesNumeric("Roll"))
  dashboard.datasetGroup("Gyro").addDataset(integrated.map(_.y.toDegrees).toTimeSeriesNumeric("Pitch"))
  dashboard.datasetGroup("Gyro").addDataset(integrated.map(_.z.toDegrees).toTimeSeriesNumeric("Yaw"))
  dashboard.start()
}

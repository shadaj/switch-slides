package me.shadaj.jinput.leobodnar

import java.awt.Robot
import java.awt.event.{InputEvent, KeyEvent}

import com.lynbrookrobotics.potassium.Signal
import com.lynbrookrobotics.potassium.clock.JavaClock
import net.java.games.input.ControllerEnvironment
import squants.space.{Angle, Degrees}
import squants.time.Milliseconds

object Main extends App {
  implicit val env = ControllerEnvironment.getDefaultEnvironment
  val robot = new Robot()

  val right = new JoyConRight

  implicit val clock = JavaClock

  implicit val polling = JavaClock.periodicEvent(Milliseconds(10))

  val rightA = Signal(right.A.isDown)
  val rightAPressed = rightA.filter(_ == true)

  val rightB = Signal(right.B.isDown)
  val rightBPressed = rightB.filter(identity)

  // Slide Control
  rightAPressed.onStart.foreach(() => {
    robot.keyPress(KeyEvent.VK_RIGHT)
    robot.keyRelease(KeyEvent.VK_RIGHT)
  })

  rightBPressed.onStart.foreach(() => {
    robot.keyPress(KeyEvent.VK_LEFT)
    robot.keyRelease(KeyEvent.VK_LEFT)
  })

  // Mouse
  val rawPov = Signal(right.Pov.rotation)
  val povMoving = rawPov.map(_ != 0f)

  def povToAngle(pov: Float): Angle = {
    val degreesFromBottom = pov * Degrees(360)
    Degrees(360) - (degreesFromBottom + Degrees(90))
  }

  val povAngle = rawPov.map(povToAngle)

  val mouseSpeed = 5D

  val mouseVelocity = povAngle.zip(povMoving).map { case (a, moving) =>
    if (moving) {
      (math.cos(a.toRadians) * mouseSpeed, -math.sin(a.toRadians) * mouseSpeed)
    } else {
      (0D, 0D)
    }
  }

  val mouseVelocitySmooth = mouseVelocity.toPeriodic.scanLeft((0D, 0D)) { (acc, cur, dt) =>
    ((acc._1 * 0.9) + (cur._1 * 0.1), (acc._2 * 0.9) + (cur._2 * 0.1))
  }

  val mousePosition = mouseVelocitySmooth.scanLeft((0D, 0D)) { (acc, cur, dt) =>
    (acc._1 + cur._1, acc._2 + cur._2)
  }.toPollingSignal(Milliseconds(10)).map(_.getOrElse((0D, 0D)))

  def abs(vector: (Double, Double)): Double = {
    math.sqrt(vector._1 * vector._1 + vector._2 * vector._2)
  }

  mouseVelocitySmooth.peek.filter(o => o.map(abs).getOrElse(0D) > 0.1D).foreach(() => {
    val (x, y) = mousePosition.get
    robot.mouseMove(x.toInt, y.toInt)
  })

  // Clicking
  val clickButton = Signal(right.ZR.isDown)
  val clickButtonPressed = clickButton.filter(identity)

  clickButtonPressed.onStart.foreach(() => {
    robot.mousePress(InputEvent.BUTTON1_DOWN_MASK)
  })

  clickButtonPressed.onEnd.foreach(() => {
    robot.mouseRelease(InputEvent.BUTTON1_DOWN_MASK)
  })
}

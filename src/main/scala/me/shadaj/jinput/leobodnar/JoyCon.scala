package me.shadaj.jinput.leobodnar

import net.java.games.input.{Component, ControllerEnvironment}

class JoyConButton(component: Component) {
  def isDown: Boolean = {
    component.getPollData > 0.5
  }
}

class JoyConPOV(component: Component) {
  def rotation: Float = {
    component.getPollData
  }
}

class JoyCon(name: String)(implicit env: ControllerEnvironment) {
  protected val controller = env.getControllers.find(_.getName == name).get

  println(controller.getClass)

  new Thread(() => {
    while (!Thread.interrupted) {
      controller.poll()
      Thread.sleep(10)
    }
  }).start()
}

class JoyConLeft(implicit env: ControllerEnvironment) extends JoyCon("Joy-Con (L)")(env) {

}

class JoyConRight(implicit env: ControllerEnvironment) extends JoyCon("Joy-Con (R)")(env) {
  val A = new JoyConButton(controller.getComponent(Component.Identifier.Button._0))
  val B = new JoyConButton(controller.getComponent(Component.Identifier.Button._2))
  val Pad = new JoyConButton(controller.getComponent(Component.Identifier.Button._11))
  val ZR = new JoyConButton(controller.getComponent(Component.Identifier.Button._15))

  val Pov = new JoyConPOV(controller.getComponent(Component.Identifier.Axis.POV))
}

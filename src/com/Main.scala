package com

import java.awt.Graphics2D
import java.awt.event.{KeyAdapter, KeyEvent, KeyListener, MouseWheelEvent, MouseWheelListener}
import javax.swing.{JFrame, WindowConstants}
import KeyEvent._

object Main {

  def draw(g: Graphics2D): Unit = {
    Render3D.render()
    g.drawImage(Render3D.image, 0, 0, null)
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame()
    frame.setSize(Render3D.width, Render3D.height)
    frame.setUndecorated(false)
    frame.setTitle("Scala Render")
    frame.addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = Main.keyPressed(e)
    })
    frame.addMouseWheelListener(Main.mouseWheelMoved)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.createBufferStrategy(2)
    while (true) {
      val frameLength = 1000 / 60
      val start = System.currentTimeMillis
      val bs = frame.getBufferStrategy
      val g = bs.getDrawGraphics.asInstanceOf[Graphics2D]
      g.clearRect(0, 0, frame.getWidth, frame.getHeight)
      draw(g)
      bs.show()
      g.dispose()
      val end = System.currentTimeMillis
      val len = end - start
      if (len < frameLength) Thread.sleep(frameLength - len)
    }
  }

  def keyPressed(e: KeyEvent): Unit = {
    val d = moveDirection(e.getKeyCode)
    if (e.isAltDown) Render3D.light = Render3D.light + d * 5
    else if (e.isControlDown) Render3D.translation = Render3D.translation + d * 5
    else if (e.isShiftDown) Render3D.rotation = Render3D.rotation + d * (math.Pi / 20)
    else Render3D.cameraCenter = Render3D.cameraCenter + d * 5
  }

  def moveDirection(code: Int): Vector3D = {
    if (code == VK_A) return Vector3D(-1, 0, 0)
    if (code == VK_D) return Vector3D(1, 0, 0)
    if (code == VK_S) return Vector3D(0, 0, -1)
    if (code == VK_W) return Vector3D(0, 0, 1)
    if (code == VK_Q) return Vector3D(0, -1, 0)
    if (code == VK_E) return Vector3D(0, 1, 0)
    Vector3D(0, 0, 0)
  }

  def mouseWheelMoved(e: MouseWheelEvent): Unit = Render3D.scale *= math.pow(1.1, -e.getWheelRotation)
}

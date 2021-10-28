package com

import java.awt.Color
import java.awt.image.BufferedImage

object Render2D {

  def render(img: BufferedImage): Unit = {
    renderRegularPolygon(img, Vector2D(400, 400), 300, 20, Color.BLACK)
  }

  def renderLine(img: BufferedImage, a: Vector2D, b: Vector2D, color: Color): Unit = {
    val v = a to b
    val len = v.len
    val unit = v / len
    for (i <- 0 to len.toInt) renderPixel(img, a + unit * i, color)
  }

  def renderTriangle(img: BufferedImage, a: Vector2D, b: Vector2D, c: Vector2D, color: Color): Unit = {
    val x_min = 0.max(a.x.min(b.x).min(c.x).toInt)
    val x_max = img.getWidth.min(a.x.max(b.x).max(c.x).toInt)
    val y_min = 0.max(a.y.min(b.y).min(c.y).toInt)
    val y_max = img.getHeight.min(a.y.max(b.y).max(c.y).toInt)
    for (x <- x_min to x_max; y <- y_min to y_max) {
      val p = Vector2D(x, y)
      val v = asBarycentric(a, b, c, p)
      if (v.x >= 0 && v.y >= 0 && v.z >= 0) renderPixel(img, p, color)
    }
  }

  def renderTriangleBounds(img: BufferedImage, a: Vector2D, b: Vector2D, c: Vector2D, color: Color): Unit = {
    renderLine(img, a, b, color)
    renderLine(img, b, c, color)
    renderLine(img, c, a, color)
  }

  def renderRegularPolygon(img: BufferedImage, p: Vector2D, len: Double, count: Int, color: Color): Unit = {
    for (i <- 0 until count) {
      val a = p + Vector2D(Math.cos(2 * Math.PI * i / count), Math.sin(2 * Math.PI * i / count)) * len
      val b = p + Vector2D(Math.cos(2 * Math.PI * (i + 1) / count), Math.sin(2 * Math.PI * (i + 1) / count)) * len
      renderTriangle(img, a, b, p, color)
      renderTriangleBounds(img, a, b, p, Color.BLACK)
    }
  }

  def asBarycentric(a: Vector2D, b: Vector2D, c: Vector2D, p: Vector2D): Vector3D = {
    val vx = Vector3D(c.x - a.x, b.x - a.x, a.x - p.x)
    val vy = Vector3D(c.y - a.y, b.y - a.y, a.y - p.y)
    val v = vx * vy
    Vector3D(v.z - v.x - v.y, v.y, v.x) / v.z
  }

  def renderPixel(img: BufferedImage, v: Vector2D, color: Color): Unit = {
    if (0 <= v.x && v.x < img.getWidth && 0 <= v.y && v.y < img.getHeight) img.setRGB(v.xInt, v.yInt, color.getRGB)
  }

}

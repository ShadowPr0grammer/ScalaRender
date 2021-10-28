package com

case class Vector2D(x: Double, y: Double) {
  def +(ot: Vector2D): Vector2D = Vector2D(x + ot.x, y + ot.y)

  def -(ot: Vector2D): Vector2D = Vector2D(x - ot.x, y - ot.y)

  def +(k: Double): Vector2D = Vector2D(x + k, y + k)

  def -(k: Double): Vector2D = Vector2D(x - k, y - k)

  def to(v: Vector2D): Vector2D = v - this

  def *(ot: Vector2D): Double = x * ot.x + y * ot.y

  def *(k: Double): Vector2D = Vector2D(x * k, y * k)

  def /(k: Double): Vector2D = Vector2D(x / k, y / k)

  def len: Double = Math.sqrt(x * x + y * y)

  def xInt: Int = x.toInt

  def yInt: Int = y.toInt

  def unit: Vector2D = this / len

}

package com

case class Vector4D(x: Double, y: Double, z: Double, w: Double) {
  def +(ot: Vector4D): Vector4D = Vector4D(x + ot.x, y + ot.y, z + ot.z, w + ot.w)

  def -(ot: Vector4D): Vector4D = Vector4D(x - ot.x, y - ot.y, z - ot.z, w - ot.w)

  def +(k: Double): Vector4D = Vector4D(x + k, y + k, z + k, w + k)

  def -(k: Double): Vector4D = Vector4D(x - k, y - k, z - k, w - k)

  def to(ot: Vector4D): Vector4D = ot - this

  def cross(ot: Vector4D): Double = x * ot.x + y * ot.y + z * ot.z + w * ot.w

  def *(k: Double): Vector4D = Vector4D(x * k, y * k, z * k, w * k)

  def /(k: Double): Vector4D = Vector4D(x / k, y / k, z / k, w / k)

  def len: Double = Math.sqrt(x * x + y * y + z * z + w * w)

  def xInt: Int = x.toInt

  def yInt: Int = y.toInt

  def zInt: Int = z.toInt

  def wInt: Int = w.toInt

  def unit: Vector4D = this / len

  def to3d: Vector3D = /*if (w == 0) */Vector3D(x, y, z)/* else Vector3D(x / w, y / w, z / w)*/
}

package com

case class Vector3D(x: Double, y: Double, z: Double) {

  def +(ot: Vector3D): Vector3D = Vector3D(x + ot.x, y + ot.y, z + ot.z)

  def -(ot: Vector3D): Vector3D = Vector3D(x - ot.x, y - ot.y, z - ot.z)

  def +(k: Double): Vector3D = Vector3D(x + k, y + k, z + k)

  def -(k: Double): Vector3D = Vector3D(x - k, y - k, z - k)

  def to(ot: Vector3D): Vector3D = ot - this

  def cross(ot: Vector3D): Double = x * ot.x + y * ot.y + z * ot.z

  def *(k: Double): Vector3D = Vector3D(x * k, y * k, z * k)

  def /(k: Double): Vector3D = Vector3D(x / k, y / k, z / k)

  def len: Double = Math.sqrt(x * x + y * y + z * z)

  def xInt: Int = x.toInt

  def yInt: Int = y.toInt

  def zInt: Int = z.toInt

  def unit: Vector3D = this / len

  def *(ot: Vector3D): Vector3D = Vector3D(y * ot.z - z * ot.y, z * ot.x - x * ot.z, x * ot.y - y * ot.x)

  def as4dPoint: Vector4D = Vector4D(x, y, z, 1)

  def as4dVector: Vector4D = Vector4D(x, y, z, 0)

}

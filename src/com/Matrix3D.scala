package com

case class Matrix3D(a: Array[Array[Double]]) {

  def +(ot: Matrix3D): Matrix3D = {
    val newArray = Array.ofDim[Double](3, 3)
    for (i <- 0 until 3; j <- 0 until 3) newArray(i)(j) = a(i)(j) + ot.a(i)(j)
    Matrix3D(newArray)
  }

  def -(ot: Matrix3D): Matrix3D = {
    val newArray = Array.ofDim[Double](3, 3)
    for (i <- 0 until 3; j <- 0 until 3) newArray(i)(j) = a(i)(j) - ot.a(i)(j)
    Matrix3D(newArray)
  }

  def *(k: Double): Matrix3D = {
    val newArray = Array.ofDim[Double](3, 3)
    for (i <- 0 until 3; j <- 0 until 3) newArray(i)(j) = k * a(i)(j)
    Matrix3D(newArray)
  }

  def *(v: Vector3D): Vector3D = {
    val x = a(0)(0) * v.x + a(0)(1) * v.y + a(0)(2) * v.z
    val y = a(1)(0) * v.x + a(1)(1) * v.y + a(1)(2) * v.z
    val z = a(2)(0) * v.x + a(2)(1) * v.y + a(2)(2) * v.z
    Vector3D(x, y, z)
  }

  def *(ot: Matrix3D): Matrix3D = {
    val newArray = Array.ofDim[Double](3, 3)
    for (i <- 0 until 3; j <- 0 until 3; r <- 0 until 3) newArray(i)(j) += a(i)(r) * ot.a(r)(j)
    Matrix3D(newArray)
  }
}

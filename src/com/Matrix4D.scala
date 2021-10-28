package com

case class Matrix4D(a: Array[Array[Double]]) extends Function[Int, Array[Double]] {
  
  override def apply(ind: Int): Array[Double] = a(ind)

  def +(ot: Matrix4D): Matrix4D = {
    val newArray = Matrix4D.buildArray()
    for (i <- 0 until 4; j <- 0 until 4) newArray(i)(j) = a(i)(j) + ot.a(i)(j)
    Matrix4D(newArray)
  }

  def -(ot: Matrix4D): Matrix4D = {
    val newArray = Matrix4D.buildArray()
    for (i <- 0 until 4; j <- 0 until 4) newArray(i)(j) = a(i)(j) - ot.a(i)(j)
    Matrix4D(newArray)
  }

  def *(k: Double): Matrix4D = {
    val newArray = Matrix4D.buildArray()
    for (i <- 0 until 4; j <- 0 until 4) newArray(i)(j) = k * a(i)(j)
    Matrix4D(newArray)
  }

  def /(k: Double): Matrix4D = this * (1 / k)

  def *(v: Vector4D): Vector4D = {
    val x = a(0)(0) * v.x + a(0)(1) * v.y + a(0)(2) * v.z + a(0)(3) * v.w
    val y = a(1)(0) * v.x + a(1)(1) * v.y + a(1)(2) * v.z + a(1)(3) * v.w
    val z = a(2)(0) * v.x + a(2)(1) * v.y + a(2)(2) * v.z + a(2)(3) * v.w
    val w = a(3)(0) * v.x + a(3)(1) * v.y + a(3)(2) * v.z + a(3)(3) * v.w
    Vector4D(x, y, z, w)
  }

  def *(ot: Matrix4D): Matrix4D = {
    val newArray = Matrix4D.buildArray()
    for (i <- 0 until 4; j <- 0 until 4; r <- 0 until 4) newArray(i)(j) += a(i)(r) * ot.a(r)(j)
    Matrix4D(newArray)
  }

  def determinant: Double = {
    a(0)(3) * a(1)(2) * a(2)(1) * a(3)(0) - a(0)(2) * a(1)(3) * a(2)(1) * a(3)(0) - a(0)(3) * a(1)(1) * a(2)(2) * a(3)(0) + a(0)(1) * a(1)(3) * a(2)(2) * a(3)(0) +
      a(0)(2) * a(1)(1) * a(2)(3) * a(3)(0) - a(0)(1) * a(1)(2) * a(2)(3) * a(3)(0) - a(0)(3) * a(1)(2) * a(2)(0) * a(3)(1) + a(0)(2) * a(1)(3) * a(2)(0) * a(3)(1) +
      a(0)(3) * a(1)(0) * a(2)(2) * a(3)(1) - a(0)(0) * a(1)(3) * a(2)(2) * a(3)(1) - a(0)(2) * a(1)(0) * a(2)(3) * a(3)(1) + a(0)(0) * a(1)(2) * a(2)(3) * a(3)(1) +
      a(0)(3) * a(1)(1) * a(2)(0) * a(3)(2) - a(0)(1) * a(1)(3) * a(2)(0) * a(3)(2) - a(0)(3) * a(1)(0) * a(2)(1) * a(3)(2) + a(0)(0) * a(1)(3) * a(2)(1) * a(3)(2) +
      a(0)(1) * a(1)(0) * a(2)(3) * a(3)(2) - a(0)(0) * a(1)(1) * a(2)(3) * a(3)(2) - a(0)(2) * a(1)(1) * a(2)(0) * a(3)(3) + a(0)(1) * a(1)(2) * a(2)(0) * a(3)(3) +
      a(0)(2) * a(1)(0) * a(2)(1) * a(3)(3) - a(0)(0) * a(1)(2) * a(2)(1) * a(3)(3) - a(0)(1) * a(1)(0) * a(2)(2) * a(3)(3) + a(0)(0) * a(1)(1) * a(2)(2) * a(3)(3)
  }

  def transpose: Matrix4D = {
    val newArray = Matrix4D.buildArray()
    for (i <- 0 until 4; j <- 0 until 4) newArray(i)(j) = a(j)(i)
    Matrix4D(newArray)
  }

  def inverse: Matrix4D = {
    val newArray: Array[Array[Double]] = Matrix4D.buildArray()
    newArray(0)(0) = a(1)(2) * a(2)(3) * a(3)(1) - a(1)(3) * a(2)(2) * a(3)(1) + a(1)(3) * a(2)(1) * a(3)(2) - a(1)(1) * a(2)(3) * a(3)(2) - a(1)(2) * a(2)(1) * a(3)(3) + a(1)(1) * a(2)(2) * a(3)(3)
    newArray(0)(1) = a(0)(3) * a(2)(2) * a(3)(1) - a(0)(2) * a(2)(3) * a(3)(1) - a(0)(3) * a(2)(1) * a(3)(2) + a(0)(1) * a(2)(3) * a(3)(2) + a(0)(2) * a(2)(1) * a(3)(3) - a(0)(1) * a(2)(2) * a(3)(3)
    newArray(0)(2) = a(0)(2) * a(1)(3) * a(3)(1) - a(0)(3) * a(1)(2) * a(3)(1) + a(0)(3) * a(1)(1) * a(3)(2) - a(0)(1) * a(1)(3) * a(3)(2) - a(0)(2) * a(1)(1) * a(3)(3) + a(0)(1) * a(1)(2) * a(3)(3)
    newArray(0)(3) = a(0)(3) * a(1)(2) * a(2)(1) - a(0)(2) * a(1)(3) * a(2)(1) - a(0)(3) * a(1)(1) * a(2)(2) + a(0)(1) * a(1)(3) * a(2)(2) + a(0)(2) * a(1)(1) * a(2)(3) - a(0)(1) * a(1)(2) * a(2)(3)
    newArray(1)(0) = a(1)(3) * a(2)(2) * a(3)(0) - a(1)(2) * a(2)(3) * a(3)(0) - a(1)(3) * a(2)(0) * a(3)(2) + a(1)(0) * a(2)(3) * a(3)(2) + a(1)(2) * a(2)(0) * a(3)(3) - a(1)(0) * a(2)(2) * a(3)(3)
    newArray(1)(1) = a(0)(2) * a(2)(3) * a(3)(0) - a(0)(3) * a(2)(2) * a(3)(0) + a(0)(3) * a(2)(0) * a(3)(2) - a(0)(0) * a(2)(3) * a(3)(2) - a(0)(2) * a(2)(0) * a(3)(3) + a(0)(0) * a(2)(2) * a(3)(3)
    newArray(1)(2) = a(0)(3) * a(1)(2) * a(3)(0) - a(0)(2) * a(1)(3) * a(3)(0) - a(0)(3) * a(1)(0) * a(3)(2) + a(0)(0) * a(1)(3) * a(3)(2) + a(0)(2) * a(1)(0) * a(3)(3) - a(0)(0) * a(1)(2) * a(3)(3)
    newArray(1)(3) = a(0)(2) * a(1)(3) * a(2)(0) - a(0)(3) * a(1)(2) * a(2)(0) + a(0)(3) * a(1)(0) * a(2)(2) - a(0)(0) * a(1)(3) * a(2)(2) - a(0)(2) * a(1)(0) * a(2)(3) + a(0)(0) * a(1)(2) * a(2)(3)
    newArray(2)(0) = a(1)(1) * a(2)(3) * a(3)(0) - a(1)(3) * a(2)(1) * a(3)(0) + a(1)(3) * a(2)(0) * a(3)(1) - a(1)(0) * a(2)(3) * a(3)(1) - a(1)(1) * a(2)(0) * a(3)(3) + a(1)(0) * a(2)(1) * a(3)(3)
    newArray(2)(1) = a(0)(3) * a(2)(1) * a(3)(0) - a(0)(1) * a(2)(3) * a(3)(0) - a(0)(3) * a(2)(0) * a(3)(1) + a(0)(0) * a(2)(3) * a(3)(1) + a(0)(1) * a(2)(0) * a(3)(3) - a(0)(0) * a(2)(1) * a(3)(3)
    newArray(2)(2) = a(0)(1) * a(1)(3) * a(3)(0) - a(0)(3) * a(1)(1) * a(3)(0) + a(0)(3) * a(1)(0) * a(3)(1) - a(0)(0) * a(1)(3) * a(3)(1) - a(0)(1) * a(1)(0) * a(3)(3) + a(0)(0) * a(1)(1) * a(3)(3)
    newArray(2)(3) = a(0)(3) * a(1)(1) * a(2)(0) - a(0)(1) * a(1)(3) * a(2)(0) - a(0)(3) * a(1)(0) * a(2)(1) + a(0)(0) * a(1)(3) * a(2)(1) + a(0)(1) * a(1)(0) * a(2)(3) - a(0)(0) * a(1)(1) * a(2)(3)
    newArray(3)(0) = a(1)(2) * a(2)(1) * a(3)(0) - a(1)(1) * a(2)(2) * a(3)(0) - a(1)(2) * a(2)(0) * a(3)(1) + a(1)(0) * a(2)(2) * a(3)(1) + a(1)(1) * a(2)(0) * a(3)(2) - a(1)(0) * a(2)(1) * a(3)(2)
    newArray(3)(1) = a(0)(1) * a(2)(2) * a(3)(0) - a(0)(2) * a(2)(1) * a(3)(0) + a(0)(2) * a(2)(0) * a(3)(1) - a(0)(0) * a(2)(2) * a(3)(1) - a(0)(1) * a(2)(0) * a(3)(2) + a(0)(0) * a(2)(1) * a(3)(2)
    newArray(3)(2) = a(0)(2) * a(1)(1) * a(3)(0) - a(0)(1) * a(1)(2) * a(3)(0) - a(0)(2) * a(1)(0) * a(3)(1) + a(0)(0) * a(1)(2) * a(3)(1) + a(0)(1) * a(1)(0) * a(3)(2) - a(0)(0) * a(1)(1) * a(3)(2)
    newArray(3)(3) = a(0)(1) * a(1)(2) * a(2)(0) - a(0)(2) * a(1)(1) * a(2)(0) + a(0)(2) * a(1)(0) * a(2)(1) - a(0)(0) * a(1)(2) * a(2)(1) - a(0)(1) * a(1)(0) * a(2)(2) + a(0)(0) * a(1)(1) * a(2)(2)
    Matrix4D(newArray) / determinant
  }

}

object Matrix4D {

  def buildArray(): Array[Array[Double]] = Array.ofDim[Double](4, 4)

  def buildNew(): Matrix4D = Matrix4D(buildArray())

  def identity(): Matrix4D = {
    val identity = Matrix4D.buildNew()
    for (i <- 0 until 4) identity(i)(i) = 1
    identity
  }

}

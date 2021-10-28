package com

case class Matrix2D(a: Array[Array[Double]]) {

  def +(ot: Matrix2D): Matrix2D = {
    val newArray = Array.ofDim[Double](2, 2)
    for (i <- 0 until 2; j <- 0 until 2) newArray(i)(j) = a(i)(j) + ot.a(i)(j)
    Matrix2D(newArray)
  }

  def -(ot: Matrix2D): Matrix2D = {
    val newArray = Array.ofDim[Double](2, 2)
    for (i <- 0 until 2; j <- 0 until 2) newArray(i)(j) = a(i)(j) - ot.a(i)(j)
    Matrix2D(newArray)
  }

  def *(k: Double): Matrix2D = {
    val newArray = Array.ofDim[Double](2, 2)
    for (i <- 0 until 2; j <- 0 until 2) newArray(i)(j) = k * a(i)(j)
    Matrix2D(newArray)
  }

  def *(v: Vector2D): Vector2D = {
    val x = a(0)(0) * v.x + a(0)(1) * v.y
    val y = a(1)(0) * v.x + a(1)(1) * v.y
    Vector2D(x, y)
  }

  def *(ot: Matrix2D): Matrix2D = {
    val newArray = Array.ofDim[Double](2, 2)
    for (i <- 0 until 2; j <- 0 until 2; r <- 0 until 2) newArray(i)(j) += a(i)(r) * ot.a(r)(j)
    Matrix2D(newArray)
  }
}

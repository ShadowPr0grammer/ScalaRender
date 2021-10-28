package com

import java.awt.image.BufferedImage
import java.awt.{Color, Toolkit}
import scala.math.{Pi, cos, max, sin}

object Render3D {

  val width: Int = Toolkit.getDefaultToolkit.getScreenSize.width
  val height: Int = Toolkit.getDefaultToolkit.getScreenSize.height
  val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  val zBuffer: Array[Double] = Array.fill[Double](width * height)(Double.NegativeInfinity)
  var light: Vector3D = Vector3D(0, 0, 10)
  var scale: Double = 1.0
  var translation: Vector3D = Vector3D(0, -width, -height / 2)
  var rotation: Vector3D = Vector3D(0, 0, 0)
  var cameraCenter: Vector3D = Vector3D(0, 0, 0)
  var cameraEye: Vector2D = Vector2D(0, 1)
  var cameraDistance: Double = 5
  var cameraAngle: Double = Pi / 2
  
  val UAZ: Obj = Obj.read("resources/uaz/uaz.obj", "resources/uaz/uaz_med_white_d.png")

  def render(): Unit = {
    renderObj(UAZ)
  }

  def asBarycentric(a: Vector3D, b: Vector3D, c: Vector3D, p: Vector2D): Vector3D = {
    val vx = Vector3D(c.x - a.x, b.x - a.x, a.x - p.x)
    val vy = Vector3D(c.y - a.y, b.y - a.y, a.y - p.y)
    val v = vx * vy
    Vector3D(v.z - v.x - v.y, v.y, v.x) / v.z
  }

  def renderTriangle(a: Vector3D, b: Vector3D, c: Vector3D, colorMap: Vector3D => Int): Unit = {
    val x_min = 0.max(a.x.min(b.x).min(c.x).toInt)
    val x_max = (image.getWidth - 1).min(a.x.max(b.x).max(c.x).toInt)
    val y_min = 0.max(a.y.min(b.y).min(c.y).toInt)
    val y_max = (image.getHeight - 1).min(a.y.max(b.y).max(c.y).toInt)
    for (x <- x_min to x_max; y <- y_min to y_max) {
      val p = Vector2D(x, y)
      val bar = asBarycentric(a, b, c, p)
      if (bar.x >= 0 && bar.y >= 0 && bar.z >= 0) {
        val z = a.z * bar.x + b.z * bar.y + c.z * bar.z
        val ind = x + y * image.getWidth
        if (zBuffer(ind) < z) {
          zBuffer(ind) = z
          renderPixel(p, colorMap(bar))
        }
      }
    }
  }

  def renderObj(obj: Obj): Unit = {
    for(i <- 0 until width * height) {
      zBuffer(i) = Double.NegativeInfinity
      image.setRGB(i % width, i / width, 0)
    }
    val center = obj.getCenter
    val lightDirection = light.unit
    val eye: Vector3D = cameraCenter + Vector3D(cos(cameraEye.x) * cos(cameraEye.y),
      cos(cameraEye.x) * sin(cameraEye.y), sin(cameraEye.x)) * cameraDistance
    val modelMatrix = buildModelMatrix(scale, rotation, translation)
    val viewMatrix = buildViewMatrix(cameraCenter, eye, cameraAngle)
    val cameraDirection = cameraCenter to eye
    val projectionMatrix = buildProjectionMatrix(cameraDirection.len)
    val nModelMatrix = modelMatrix.transpose.inverse
    val screenMatrix = projectionMatrix * viewMatrix
    val matrix = screenMatrix * modelMatrix
    for (face <- obj.faces) {
      val a = obj.getVertex(face, 0) - center
      val b = obj.getVertex(face, 1) - center
      val c = obj.getVertex(face, 2) - center
      val aNormal = obj.getNormal(face, 0)
      val bNormal = obj.getNormal(face, 1)
      val cNormal = obj.getNormal(face, 2)
      val aWorld = (modelMatrix * a.as4dPoint).to3d
      val bWorld = (modelMatrix * b.as4dPoint).to3d
      val cWorld = (modelMatrix * c.as4dPoint).to3d
      val aScreen = (matrix * a.as4dPoint).to3d
      val bScreen = (matrix * b.as4dPoint).to3d
      val cScreen = (matrix * c.as4dPoint).to3d
      val aWorldNormal = (nModelMatrix * aNormal.as4dVector).to3d.unit
      val bWorldNormal = (nModelMatrix * bNormal.as4dVector).to3d.unit
      val cWorldNormal = (nModelMatrix * cNormal.as4dVector).to3d.unit
      val aTexture = obj.getScaledTextureCoordinate(face, 0)
      val bTexture = obj.getScaledTextureCoordinate(face, 1)
      val cTexture = obj.getScaledTextureCoordinate(face, 2)
      val normal = ((cWorld - aWorld) * (bWorld - aWorld)).unit
      if (normal.cross(cameraDirection) <= 0.5) {
        renderTriangle(aScreen, bScreen, cScreen, bar => {
          val color = new Color(obj.getRGB(aTexture * bar.x + bTexture * bar.y + cTexture * bar.z))
          val normalLight = aWorldNormal.cross(lightDirection) * bar.x +
            bWorldNormal.cross(lightDirection) * bar.y + cWorldNormal.cross(lightDirection) * bar.z
          val l = max(normalLight, 0.2)
          new Color((color.getRed * l).toInt, (color.getGreen * l).toInt, (color.getBlue * l).toInt).getRGB
        })
      }
    }
  }

  def renderPixel(v: Vector2D, color: Int): Unit = image.setRGB(v.xInt, v.yInt, color)

  def buildModelMatrix(scale: Double, rotation: Vector3D, translation: Vector3D): Matrix4D = {
    val scaleMatrix = Matrix4D.identity() * scale
    scaleMatrix(3)(3) = 1
    val xRotation = Matrix4D.buildNew()
    xRotation(0)(0) = 1
    xRotation(1)(1) = cos(rotation.x)
    xRotation(1)(2) = -sin(rotation.x)
    xRotation(2)(1) = sin(rotation.x)
    xRotation(2)(2) = cos(rotation.x)
    xRotation(3)(3) = 1
    val yRotation = Matrix4D.buildNew()
    yRotation(0)(0) = cos(rotation.y)
    yRotation(1)(1) = 1
    yRotation(0)(2) = -sin(rotation.y)
    yRotation(2)(0) = sin(rotation.y)
    yRotation(2)(2) = cos(rotation.y)
    yRotation(3)(3) = 1
    val zRotation = Matrix4D.buildNew()
    zRotation(0)(0) = cos(rotation.z)
    zRotation(0)(1) = -sin(rotation.z)
    zRotation(1)(0) = sin(rotation.z)
    zRotation(1)(1) = cos(rotation.z)
    zRotation(2)(2) = 1
    zRotation(3)(3) = 1
    val rotationMatrix = xRotation * yRotation * zRotation
    val translationMatrix = Matrix4D.buildNew()
    translationMatrix(0)(0) = 1
    translationMatrix(1)(1) = 1
    translationMatrix(2)(2) = 1
    translationMatrix(3)(3) = 1
    translationMatrix(0)(3) = translation.x
    translationMatrix(1)(3) = translation.y
    translationMatrix(2)(3) = translation.z
    translationMatrix * rotationMatrix * scaleMatrix
  }

  def buildProjectionMatrix(c: Double): Matrix4D = {
    val projection = Matrix4D.buildNew()
    projection(0)(0) = 1
    projection(1)(1) = 1
    projection(2)(2) = 1
    projection(3)(3) = 1
    projection(3)(2) = -1 / c
    projection
  }

  def buildViewMatrix(center: Vector3D, eye: Vector3D, angle: Double): Matrix4D = {
    val up = (eye to center) * Vector3D(cos(angle + Pi / 2), sin(angle + Pi / 2), 0)
    val vz = (center to eye).unit
    val vx = (up * vz).unit
    val vy = (vz * vx).unit
    val a = Matrix4D.buildNew()
    a(0)(0) = vx.x
    a(0)(1) = vx.y
    a(0)(2) = vx.z
    a(1)(0) = vy.x
    a(1)(1) = vy.y
    a(1)(2) = vy.z
    a(2)(0) = vz.x
    a(2)(2) = vz.y
    a(2)(3) = vz.z
    a(3)(3) = 1
    val b = Matrix4D.buildNew()
    b(0)(0) = 1
    b(1)(1) = 1
    b(2)(2) = 1
    b(3)(3) = 1
    b(0)(3) = -center.x
    b(1)(3) = -center.y
    b(2)(3) = -center.z
    a * b
  }

}

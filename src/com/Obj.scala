package com

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{BufferedReader, File, FileReader}
import javax.imageio.ImageIO
import scala.collection.mutable

class Obj {
  var texture: BufferedImage = Obj.defaultTexture
  var vertices: mutable.Buffer[Vector3D] = mutable.Buffer()
  var textureCoordinates: mutable.Buffer[Vector2D] = mutable.Buffer()
  var normals: mutable.Buffer[Vector3D] = mutable.Buffer()
  var faces: mutable.Buffer[Array[Array[Int]]] = mutable.Buffer()

  def getVertex(face: Array[Array[Int]], ind: Int): Vector3D = vertices(face(ind)(0) - 1)

  def getTextureCoordinate(face: Array[Array[Int]], ind: Int): Vector2D = textureCoordinates(face(ind)(1) - 1)

  def getScaledTextureCoordinate(face: Array[Array[Int]], ind: Int): Vector2D = {
    val c = getTextureCoordinate(face, ind)
    Vector2D(c.x * texture.getWidth, (1 - c.y) * texture.getHeight)
  }

  def getNormal(face: Array[Array[Int]], ind: Int): Vector3D = normals(face(ind)(2) - 1)

  def getCenter: Vector3D = vertices.reduce(_ + _) / vertices.length

  def getRGB(v: Vector2D): Int = texture.getRGB(0.max(v.xInt).min(texture.getWidth - 1), 0.max(v.yInt).min(texture.getHeight - 1))
}

object Obj {

  val defaultTexture = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB)

  {
    val rgb = new Color(200, 200, 200).getRGB
    for (i <- 0 until defaultTexture.getWidth; j <- 0 until defaultTexture.getHeight) defaultTexture.setRGB(i, j, rgb)
  }

  def read(fileName: String): Obj = read(new File(fileName))

  def read(file: File): Obj = read(file, defaultTexture)

  def read(fileName: String, image: String): Obj = read(new File(fileName), ImageIO.read(new File(image)))

  def read(file: File, image: BufferedImage): Obj = {
    val obj = new Obj()
    obj.texture = image
    val reader = new BufferedReader(new FileReader(file))
    var line = reader.readLine()
    while (line != null) {
      if (line.startsWith("f")) {
        line = line.substring(2)
        val arr = line.split(" ").map(s => s.split("/").map(s => s.toInt))
        obj.faces += arr
      } else if (line.startsWith("vn")) {
        line = line.substring(3)
        val arr = line.split(" ").map(s => s.toDouble)
        obj.normals += Vector3D(arr(0), arr(1), arr(2))
      } else if (line.startsWith("vt")) {
        line = line.substring(3)
        val arr = line.split(" ").map(s => s.toDouble)
        obj.textureCoordinates += Vector2D(arr(0), arr(1))
      } else if (line.startsWith("v")) {
        line = line.substring(2)
        val arr = line.split(" ").map(s => s.toDouble)
        obj.vertices += Vector3D(arr(0), arr(1), arr(2))
      }
      line = reader.readLine()
    }
    obj
  }

}

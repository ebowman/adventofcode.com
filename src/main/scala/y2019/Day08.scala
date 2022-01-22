package y2019

import scala.annotation.tailrec

trait Day08 {

  def parse(rows: Int, cols: Int, str: String): Array[Array[Array[Int]]] = {
    val layerCount = str.length / (rows * cols)
    val layers = Array.ofDim[Int](layerCount, rows, cols)
    var cursor = 0
    for
      layer <- 0 until layerCount
      row <- 0 until rows
      col <- 0 until cols
    do {
      layers(layer)(row)(col) = str.charAt(cursor) - '0'
      cursor += 1
    }
    layers
  }

  def part1(rows: Int, cols: Int, str: String): Int = {
    val layers = parse(rows, cols, str)
    val resultLayer = layers(layers.zipWithIndex.map { case (layer, index) =>
      (layer.map(_.count(_ == 0)).sum, index)
    }.minBy(_._1)._2)
    resultLayer.map(_.count(_ == 1)).sum * resultLayer.map(_.count(_ == 2)).sum
  }

  def part2(rows: Int, cols: Int, str: String): String = {
    @tailrec def process(row: Int, col: Int, layers: Array[Array[Array[Int]]], layer: Int = 0): Int = {
      val pixel = layers(layer)(row)(col)
      if pixel == 2 then process(row, col, layers, layer + 1)
      else pixel
    }

    val layers = parse(rows, cols, str)
    val dest = Array.ofDim[Int](rows, cols)
    for
      row <- 0 until rows
      col <- 0 until cols
    do dest(row)(col) = process(row, col, layers)
    dest.map(_.toSeq.map(Map(0 -> " ", 1 -> "#")).mkString).mkString("\n")
  }
}

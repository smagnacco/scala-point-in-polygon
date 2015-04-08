/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Darel Rex Finley, Sergio Magnacco
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.opensource.gis.polygon

import scala.annotation.tailrec

/**
 * Source: http://alienryderflex.com/polygon/
 *
 * Third algorithm with precalc optimization
 */

case class GeoPoint(latitude: Double, longitude: Double)

case class Polygon(points: List[GeoPoint]) {
  def corners = points.size
  def horizontalCoordinates = points map (_.latitude)
  def verticalCoordinates = points map (_.longitude)
}

object PolygonUtils {
  def pointInPolygon(point: GeoPoint, polygon: Polygon): Boolean = {

      @tailrec
      def precalc(polyCorners: Int, i: Int, j: Int, polyX: Array[Double], polyY: Array[Double],
                  constant: List[Double], multiple: List[Double]): (List[Double], List[Double])= {
        i match {
          case i if i == polyCorners => (constant, multiple)
          case i if polyY{j} == polyY{i} => precalc(polyCorners, i + 1, i, polyX, polyY, polyX{i} :: constant, 0d :: multiple)
          case i :Int => {
            val k = polyX{i} - (polyY{i} * polyX{j}) / (polyY{j} - polyY{i}) + (polyY{i} * polyX{i} ) / (polyY{j} - polyY{i})
            val m = (polyX{j} - polyX{i}) / (polyY{j} - polyY{i})
            precalc(polyCorners, i + 1, i, polyX, polyY, k :: constant, m :: multiple)
          }
        }
      }

      @tailrec
      def isInside(point: GeoPoint, polyCorners: Int, i: Int, j: Int, polyX: Array[Double], polyY: Array[Double],
                    constant: Array[Double], multiple: Array[Double], oddNodes: Boolean): Boolean = {
        val x = point.latitude
        val y = point.longitude
        i match {
          case i if i == polyCorners => oddNodes
          case i if polyY{i} < y && polyY{j} >= y || polyY{j} < y && polyY{i}>=y => {
            val odd = oddNodes ^ ( y * multiple{i} + constant{i} < x)
            isInside(point, polyCorners, i + 1, i, polyX, polyY, constant, multiple, odd)
          }
          case i: Int => isInside(point, polyCorners, i + 1, i, polyX, polyY, constant, multiple, oddNodes)
        }
      }

    val polyX: Array[Double] = polygon.horizontalCoordinates.toArray
    val polyY: Array[Double] = polygon.verticalCoordinates.toArray

    val tuple = precalc(polygon.corners, 0, polygon.corners - 1, polyX, polyY, List(), List())

    val oddNodes = false
    isInside(point, polygon.corners, 0, polygon.corners - 1, polyX, polyY, tuple._1.toArray, tuple._2.toArray, oddNodes)
  }

}

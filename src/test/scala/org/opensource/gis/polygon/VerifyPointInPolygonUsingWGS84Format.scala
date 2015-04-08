package org.opensource.gis.polygon

import org.scalatest.{FlatSpec, Matchers}

class VerifyPointInPolygonUsingWGS84Format extends FlatSpec with Matchers {

  val retiroNeighborhood = List(
    GeoPoint(-34.57643186942568, -58.38921547110658),
    GeoPoint(-34.57643186942568, -58.36320877249818),
    GeoPoint(-34.59876071694007, -58.36320877249818),
    GeoPoint(-34.59876071694007, -58.38921547110658),
    GeoPoint(-34.57643186942568, -58.38921547110658))

  val barrioNorteNeighborhood = List(
    GeoPoint(-34.57883865, -58.40449285),
    GeoPoint(-34.57883865, -58.395507150000014),
    GeoPoint(-34.58782135, -58.395507150000014),
    GeoPoint(-34.58782135, -58.40449285),
    GeoPoint(-34.57883865, -58.40449285))

  val barrioRecoletaNeighborhood = List(
    GeoPoint(-34.58286262736618, -58.40629577811342),
    GeoPoint(-34.58286262736618, -58.381919862586074),
    GeoPoint(-34.60017373322843, -58.381919862586074),
    GeoPoint(-34.60017373322843, -58.40629577811342),
    GeoPoint(-34.58286262736618, -58.40629577811342))

  val polyWantsACrackerInRetiro = Polygon(retiroNeighborhood)
  val polyWantsACrackerInBarrioNorte = Polygon(retiroNeighborhood)
  val polyWantsACrackerInRecoleta = Polygon(barrioRecoletaNeighborhood)

  "A given point inside Retiro Neighborhood in Buenos Aires" should "pointInPolygon return true" in {
    val hotel = GeoPoint(-34.594261, -58.382435)

    PolygonUtils.pointInPolygon(hotel, polyWantsACrackerInRetiro) should be (true)
  }

  "A given point outside Retiro Neighborhood in Buenos Aires" should "pointInPolygon return false" in {
    val outsideCoordinate = GeoPoint(-34.5886021,-58.389601700000014)

    PolygonUtils.pointInPolygon(outsideCoordinate, polyWantsACrackerInRetiro) should be (false)
  }

  "A given point inside Recoleta Neighborhood in Buenos Aires" should "pointInPolygon return true" in {
    val recoletaHotel = GeoPoint(-34.5886021,-58.389601700000014)

    PolygonUtils.pointInPolygon(recoletaHotel, polyWantsACrackerInRecoleta)
  }

  "A given point outside Barrio Norte Neighborhood in Buenos Aires" should "pointInPolygon return false" in {
    val hotelOutsidePoly = GeoPoint(-34.590750, -58.397187)

    PolygonUtils.pointInPolygon(hotelOutsidePoly, polyWantsACrackerInBarrioNorte) should be (false)
  }

}

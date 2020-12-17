package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.util.{Indexed, NumberUtil}

import scala.math.BigDecimal.RoundingMode

/** A DLC payout curve defined by piecewise interpolating points */
case class DLCPayoutCurve(points: Vector[OutcomePayoutPoint]) {
  require(points.init.zip(points.tail).forall {
            case (p1, p2) => p1.outcome < p2.outcome
          },
          s"Points must be ascending: $points")

  /** These points (and their indices in this.points) represent the endpoints
    * between which interpolation happens.
    * In other words these endpoints define the pieces of the piecewise function.
    */
  lazy val endpoints: Vector[Indexed[OutcomePayoutPoint]] =
    Indexed(points).filter(_.element.isEndpoint)

  /** This Vector contains the function pieces between the endpoints */
  lazy val functionComponents: Vector[DLCPayoutCurveComponent] = {
    endpoints.init.zip(endpoints.tail).map { // All pairs of adjacent endpoints
      case (Indexed(_, index), Indexed(_, nextIndex)) =>
        DLCPayoutCurveComponent(points.slice(index, nextIndex + 1))
    }
  }

  private lazy val outcomes = endpoints.map(_.element.outcome)

  /** Returns the function component on which the given oracle outcome is
    * defined, along with its index
    */
  def componentFor(outcome: BigDecimal): Indexed[DLCPayoutCurveComponent] = {
    val endpointIndex = NumberUtil.search(outcomes, outcome)
    val Indexed(endpoint, _) = endpoints(endpointIndex)

    if (
      endpoint.outcome == outcome && endpointIndex != functionComponents.length
    ) {
      Indexed(functionComponents(endpointIndex), endpointIndex)
    } else {
      Indexed(functionComponents(endpointIndex - 1), endpointIndex - 1)
    }
  }

  def getPayout(outcome: BigDecimal): Satoshis = {
    val Indexed(func, _) = componentFor(outcome)
    func(outcome)
  }

  def getPayout(outcome: BigDecimal, rounding: RoundingIntervals): Satoshis = {
    val Indexed(func, _) = componentFor(outcome)
    func(outcome, rounding)
  }

  def apply(outcome: BigDecimal): Satoshis = getPayout(outcome)

  def apply(outcome: BigDecimal, rounding: RoundingIntervals): Satoshis =
    getPayout(outcome, rounding)
}

/** A point on a DLC payout curve to be used for interpolation
  *
  * outcome: An element of the domain of possible events signed by the oracle
  * payout: The payout to the local party corresponding to outcome
  * isEndpoint: True if this point defines a boundary between pieces in the curve
  */
sealed trait OutcomePayoutPoint {
  def outcome: BigDecimal
  def payout: Satoshis
  def isEndpoint: Boolean

  def copy(
      outcome: BigDecimal = this.outcome,
      payout: Satoshis = this.payout): OutcomePayoutPoint = {
    this match {
      case OutcomePayoutEndpoint(_, _) => OutcomePayoutEndpoint(outcome, payout)
      case OutcomePayoutMidpoint(_, _) => OutcomePayoutMidpoint(outcome, payout)
    }
  }
}

object OutcomePayoutPoint {

  def apply(
      outcome: BigDecimal,
      payout: Satoshis,
      isEndpoint: Boolean): OutcomePayoutPoint = {
    if (isEndpoint) {
      OutcomePayoutEndpoint(outcome, payout)
    } else {
      OutcomePayoutMidpoint(outcome, payout)
    }
  }
}

case class OutcomePayoutEndpoint(outcome: BigDecimal, payout: Satoshis)
    extends OutcomePayoutPoint {
  override val isEndpoint: Boolean = true

  def toMidpoint: OutcomePayoutMidpoint = OutcomePayoutMidpoint(outcome, payout)
}

case class OutcomePayoutMidpoint(outcome: BigDecimal, payout: Satoshis)
    extends OutcomePayoutPoint {
  override val isEndpoint: Boolean = false

  def toEndpoint: OutcomePayoutEndpoint = OutcomePayoutEndpoint(outcome, payout)
}

/** A single piece of a larger piecewise function defined between left and right endpoints */
sealed trait DLCPayoutCurveComponent {
  def leftEndpoint: OutcomePayoutEndpoint
  def midpoints: Vector[OutcomePayoutMidpoint]
  def rightEndpoint: OutcomePayoutEndpoint

  midpoints.headOption match {
    case Some(firstMidpoint) =>
      require(leftEndpoint.outcome < firstMidpoint.outcome,
              s"Points must be ascending: $this")
      require(midpoints.init.zip(midpoints.tail).forall {
                case (m1, m2) => m1.outcome < m2.outcome
              },
              s"Points must be ascending: $this")
      require(rightEndpoint.outcome > midpoints.last.outcome,
              s"Points must be ascending: $this")
    case None =>
      require(leftEndpoint.outcome < rightEndpoint.outcome,
              s"Points must be ascending: $this")
  }

  def apply(outcome: BigDecimal): Satoshis

  def apply(outcome: BigDecimal, rounding: RoundingIntervals): Satoshis = {
    rounding.round(outcome, apply(outcome))
  }

  /** Returns the largest Long less than or equal to bd (floor function) */
  protected def bigDecimalSats(bd: BigDecimal): Satoshis = {
    Satoshis(bd.setScale(0, RoundingMode.FLOOR).toLongExact)
  }
}

object DLCPayoutCurveComponent {

  def apply(points: Vector[OutcomePayoutPoint]): DLCPayoutCurveComponent = {
    require(points.head.isEndpoint && points.last.isEndpoint,
            s"First and last points must be endpoints, $points")
    require(points.tail.init.forall(!_.isEndpoint),
            s"Endpoint detected in middle, $points")

    points match {
      case Vector(left: OutcomePayoutEndpoint, right: OutcomePayoutEndpoint) =>
        if (left.payout == right.payout) {
          OutcomePayoutConstant(left, right)
        } else {
          OutcomePayoutLine(left, right)
        }
      case Vector(left: OutcomePayoutEndpoint,
                  mid: OutcomePayoutMidpoint,
                  right: OutcomePayoutEndpoint) =>
        OutcomePayoutQuadratic(left, mid, right)
      case Vector(left: OutcomePayoutEndpoint,
                  mid1: OutcomePayoutMidpoint,
                  mid2: OutcomePayoutMidpoint,
                  right: OutcomePayoutEndpoint) =>
        OutcomePayoutCubic(left, mid1, mid2, right)
      case _ => OutcomePayoutPolynomial(points)
    }
  }
}

case class OutcomePayoutConstant(
    leftEndpoint: OutcomePayoutEndpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {
  require(leftEndpoint.payout == rightEndpoint.payout,
          "Constant function must have same values on endpoints")

  override lazy val midpoints: Vector[OutcomePayoutMidpoint] = Vector.empty

  override def apply(outcome: BigDecimal): Satoshis = leftEndpoint.payout
}

/** A Line between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutLine(
    leftEndpoint: OutcomePayoutEndpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {
  override lazy val midpoints: Vector[OutcomePayoutMidpoint] = Vector.empty

  lazy val slope: BigDecimal = {
    (rightEndpoint.payout.toLong - leftEndpoint.payout.toLong) / (rightEndpoint.outcome - leftEndpoint.outcome)
  }

  override def apply(outcome: BigDecimal): Satoshis = {
    val value =
      (outcome - leftEndpoint.outcome) * slope + leftEndpoint.payout.toLong

    bigDecimalSats(value)
  }
}

/** A quadratic between left and right endpoints defining a piece of a larger payout curve.
  * A quadratic equation defines a parabola: https://en.wikipedia.org/wiki/Quadratic_function
  */
case class OutcomePayoutQuadratic(
    leftEndpoint: OutcomePayoutEndpoint,
    midpoint: OutcomePayoutMidpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {
  override lazy val midpoints: Vector[OutcomePayoutMidpoint] = Vector(midpoint)

  private lazy val (x01, x02, x12) =
    (leftEndpoint.outcome - midpoint.outcome,
     leftEndpoint.outcome - rightEndpoint.outcome,
     midpoint.outcome - rightEndpoint.outcome)

  private lazy val (x10, x20, x21) = (-x01, -x02, -x12)

  private lazy val (y0, y1, y2) = (leftEndpoint.payout.toLong,
                                   midpoint.payout.toLong,
                                   rightEndpoint.payout.toLong)

  private lazy val (c0, c1, c2) =
    (y0 / (x01 * x02), y1 / (x10 * x12), y2 / (x20 * x21))

  override def apply(outcome: BigDecimal): Satoshis = {
    val x0 = outcome - leftEndpoint.outcome
    val x1 = outcome - midpoint.outcome
    val x2 = outcome - rightEndpoint.outcome

    val value = c0 * (x1 * x2) + c1 * (x0 * x2) + c2 * (x0 * x1)

    bigDecimalSats(value)
  }
}

/** A cubic between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutCubic(
    leftEndpoint: OutcomePayoutEndpoint,
    leftMidpoint: OutcomePayoutMidpoint,
    rightMidpoint: OutcomePayoutMidpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {

  override lazy val midpoints: Vector[OutcomePayoutMidpoint] =
    Vector(leftMidpoint, rightMidpoint)

  private lazy val (x01, x02, x03, x12, x13, x23) =
    (leftEndpoint.outcome - leftMidpoint.outcome,
     leftEndpoint.outcome - rightMidpoint.outcome,
     leftEndpoint.outcome - rightEndpoint.outcome,
     leftMidpoint.outcome - rightMidpoint.outcome,
     leftMidpoint.outcome - rightEndpoint.outcome,
     rightMidpoint.outcome - rightEndpoint.outcome)

  private lazy val (x10, x20, x30, x21, x31, x32) =
    (-x01, -x02, -x03, -x12, -x13, -x23)

  private lazy val (y0, y1, y2, y3) = (leftEndpoint.payout.toLong,
                                       leftMidpoint.payout.toLong,
                                       rightMidpoint.payout.toLong,
                                       rightEndpoint.payout.toLong)

  private lazy val (c0, c1, c2, c3) =
    (y0 / (x01 * x02 * x03),
     y1 / (x10 * x12 * x13),
     y2 / (x20 * x21 * x23),
     y3 / (x30 * x31 * x32))

  override def apply(outcome: BigDecimal): Satoshis = {
    val x0 = outcome - leftEndpoint.outcome
    val x1 = outcome - leftMidpoint.outcome
    val x2 = outcome - rightMidpoint.outcome
    val x3 = outcome - rightEndpoint.outcome

    val value =
      c0 * (x1 * x2 * x3) + c1 * (x0 * x2 * x3) + c2 * (x0 * x1 * x3) + c3 * (x0 * x1 * x2)

    bigDecimalSats(value)
  }
}

/** A polynomial interpolating points and defining a piece of a larger payout curve */
case class OutcomePayoutPolynomial(points: Vector[OutcomePayoutPoint])
    extends DLCPayoutCurveComponent {
  require(points.head.isEndpoint && points.last.isEndpoint,
          s"First and last points must be endpoints, $points")
  require(points.tail.init.forall(!_.isEndpoint),
          s"Endpoint detected in middle, $points")

  override lazy val leftEndpoint: OutcomePayoutEndpoint =
    points.head.asInstanceOf[OutcomePayoutEndpoint]

  override lazy val rightEndpoint: OutcomePayoutEndpoint =
    points.last.asInstanceOf[OutcomePayoutEndpoint]

  override lazy val midpoints: Vector[OutcomePayoutMidpoint] =
    points.tail.init.asInstanceOf[Vector[OutcomePayoutMidpoint]]

  lazy val coefficients: Vector[BigDecimal] = {
    points.map { point =>
      val xi = point.outcome
      val yi = point.payout

      val denom = points.foldLeft(BigDecimal(1)) {
        case (prodSoFar, p) =>
          val xj = p.outcome

          if (xj == xi) {
            prodSoFar
          } else {
            prodSoFar * (xi - xj)
          }
      }

      yi.toLong / denom
    }
  }

  override def apply(outcome: BigDecimal): Satoshis = {
    points.find(_.outcome == outcome) match {
      case Some(point) => point.payout
      case None =>
        val allProd = points.foldLeft(BigDecimal(1)) {
          case (prodSoFar, point) =>
            prodSoFar * (outcome - point.outcome)
        }

        val value = coefficients.zipWithIndex.foldLeft(BigDecimal(0)) {
          case (sumSoFar, (coefficientI, i)) =>
            sumSoFar + (coefficientI * allProd / (outcome - points(i).outcome))
        }

        bigDecimalSats(value)
    }
  }
}
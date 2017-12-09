package src

import java.io.{BufferedReader, FileReader}

import org.apache.commons.io.LineIterator
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.exception.{NotStrictlyPositiveException, NumberIsTooSmallException}
import org.apache.commons.math3.stat.inference.TTest

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


//return Boolean Type
class StatisticsRule {

  var reqTime: Double = 0
  var baseTime: Double = 0


  def tTest(reqArr: ArrayBuffer[Double]): Boolean = {
    var mean: Double = 0
    var t_test = new TTest
    var list = new ArrayBuffer[Double]
    reqArr.foreach(x => {
      list += x
    })

    var pv: Double = 0
    try {
      pv = t_test.tTest(mean, list.toArray)
      println(pv)
    } catch {
      case e: NumberIsTooSmallException => {
        return true
      }
    }
    if (pv < 0.05) return true

    return false
  }

  def make_NormDist(arr: ArrayBuffer[Double]): NormalDistribution = {
    var sum: Double = 0
    var mean: Double = 0
    var s: Double = 0
    var n = arr.size

    arr.foreach(sum += _)
    mean = sum / n

    sum = 0
    arr.foreach(x => {
      sum += ((x - mean) * (x - mean))
    })
    s = sum / n

    var returnValue: NormalDistribution = null

    try {
      returnValue = new NormalDistribution(mean, Math.sqrt(s))
    }
    catch {
      case e: NotStrictlyPositiveException => returnValue = new NormalDistribution(0, 1)
    }
    return returnValue
  }

  def make_NormDist(freq: Double): NormalDistribution = {
    return new NormalDistribution(freq, 1)
  }

  def make_Z(arr: ArrayBuffer[Double], base_m: Double): Double = {
    var sum: Double = 0
    var mean: Double = 0
    var s: Double = 0
    var n: Double = arr.size

    arr.foreach(sum += _)
    mean = sum / n

    sum = 0
    arr.foreach(x => {
      sum += ((x - mean) * (x - mean))
    })
    s = sum / n

    var z = (mean - base_m) / (s / Math.sqrt(n))
    return z
  }


  def zTest(req_arr: ArrayBuffer[Double], base_arr: ArrayBuffer[Double]): Double = {
    var score: Double = 0
    var sum: Double = 0

    for (i <- 0 to req_arr.size - 1) {
      var req_mean = req_arr(i)
      var nd = make_NormDist(base_arr(i))
      if (nd.getMean > req_mean) score = nd.probability(req_mean, nd.getMean) * 2
      if (nd.getMean < req_mean) score = nd.probability(nd.getMean, req_mean) * 2
      score = 1 - score

      sum += score
    }

    return sum / req_arr.size
    //    var base_normDist = make_NormDist(base_arr)
    //    var sum: Double = 0
    //    var mean: Double = 0
    //    var n:Double  = req_arr.size
    //    req_arr.foreach( sum += _ )
    //    mean = sum / n

    //    if(base_normDist.getMean > mean) score = base_normDist.probability(mean,base_normDist.getMean).toDouble
    //    if(base_normDist.getMean < mean) score = base_normDist.probability(base_normDist.getMean,mean).toDouble
    //
    //    return 1-(score*2)
  }

  //https://github.com/stkim123/kr.ac.jbnu.ssel.cwa/invitations
}

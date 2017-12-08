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

  def make_NormDist(arr:ArrayBuffer[Double]): NormalDistribution = {
    var sum: Double = 0
    var mean: Double = 0
    var s: Double = 0
    var n = arr.size

    arr.foreach( sum += _ )
    mean = sum / n

    sum = 0
    arr.foreach( x => {
      sum += ((x-mean)*(x-mean))
    })
    s = sum/n

    var returnValue:NormalDistribution = null

    try{returnValue = new NormalDistribution(mean,Math.sqrt(s))}
    catch{case e:NotStrictlyPositiveException => returnValue = new NormalDistribution(0,1)}
    return returnValue
  }

  def make_Z(arr:ArrayBuffer[Double], base_m:Double):Double = {
    var sum: Double = 0
    var mean: Double = 0
    var s: Double = 0
    var n:Double  = arr.size

    arr.foreach( sum += _ )
    mean = sum / n

    sum = 0
    arr.foreach( x => {
      sum += ((x-mean)*(x-mean))
    })
    s = sum/n

    var z = (mean-base_m)/(s/Math.sqrt(n))
    return z
  }


  def zTest(req_arr: ArrayBuffer[Double], base_arr: ArrayBuffer[Double]): Double = {
    var base_normDist = make_NormDist(base_arr)
    var sum: Double = 0
    var mean: Double = 0
    var n:Double  = req_arr.size
    req_arr.foreach( sum += _ )
    mean = sum / n

    var score:Double = 0

    if(base_normDist.getMean > mean) score = base_normDist.probability(mean,base_normDist.getMean).toDouble
    if(base_normDist.getMean < mean) score = base_normDist.probability(base_normDist.getMean,mean).toDouble

//    var base_sum:Double = 0
//    var base_m:Double = 0
//    base_arr.foreach( base_sum += _ )
//    base_m = base_sum / base_arr.size
//    var requset_Z = make_Z(req_arr,base_m)
//
//    var score:Double = 0
//    val dnst = base_normDist.density(requset_Z)
//    score = (1 - (0.5 - base_normDist.density(requset_Z)))
//    println("score  ::  "+dnst.toInt)
    return 1-score
  }

  //https://github.com/stkim123/kr.ac.jbnu.ssel.cwa/invitations
}

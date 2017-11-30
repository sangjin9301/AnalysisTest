package src

import java.io.{BufferedReader, FileReader}

import org.apache.commons.io.LineIterator
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.stat.inference.TTest

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


//return Boolean Type
class StatisticsRule {

  var reqTime: Double = 0
  var baseTime: Double = 0


  def tTest(reqArr: ArrayBuffer[Int]): (Boolean, Double) = {
    var result: Boolean = false
    var mean: Double = 0
    var t_test = new TTest
    var list = new ArrayBuffer[Double]
    reqArr.foreach(x => {
      list += x.toDouble
    })

    var pv = t_test.tTest(mean, list.toArray)

    if (pv < 0.05) result = true

    return (result, pv)
  }


  def zTest(arr: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[Double] = {
    var norm = new NormalDistribution // mean 0
    var i: Int = 0
    var score = new ArrayBuffer[Double]
    while (i < arr.size) {
      var j: Int = 0
      var oneDayArray = arr(i)
      var sum: Double = 0
      var mean: Double = 0
      var s: Double = 0
      var n = oneDayArray.size
      while (j < n) {
        sum += oneDayArray(j)
        j += 1
      }
      mean = sum / n
      if (n >= 30) score += (1 - (0.5 - norm.cumulativeProbability(mean)).abs)
      //var z = (mean-0)/(s/Math.sqrt(n))// n의 제곱근은 표준오차
      //z를 정규분포에 대입해서 0.5와의 차이를 1에서 뺌 = 점수
      i += 1
    }
    return score
  }

//https://github.com/stkim123/kr.ac.jbnu.ssel.cwa/invitations
}

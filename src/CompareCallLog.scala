package src

import java.io.{BufferedReader, FileReader}

import org.apache.commons.io.LineIterator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompareCallLog {
  def excute(path: String, reqTime: Double, baseTime: Double): (Boolean, Double) = {
    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(path))
    var st = new StatisticsRule
    val iter = new LineIterator(bufReader)
    var reqMap = new mutable.HashMap[String, Int]
    var baseMap = new mutable.HashMap[String, Int]

    //csv파일에서 데이터 불러오기
    var LineList = new ArrayBuffer[Array[String]]
    while (iter.hasNext) {
      LineList += iter.nextLine.split(",")
    }

    LineList.remove(0)

    LineList.foreach(x => {
      reqMap.put(x(4), 0)
      baseMap.put(x(4), 0)
    })


    var reqList = new ArrayBuffer[Int]


    var todayWeek = LineList.last(2)
    LineList.foreach(data => {
      if (data(2).equals(todayWeek)) {


        if (data(1).toDouble > (LineList.last(1).toDouble - reqTime)) {
          var key = data(4)
          if (reqMap.contains(key)) {
            var count: Int = reqMap.get(key).get
            reqMap.put(key, count + 1)
          } else {
            reqMap.put(key, 1)
          }
        }


        if (data(1).toDouble < (LineList.last(1).toDouble - reqTime)) {
          if (data(1).toDouble > (LineList.last(1).toDouble - baseTime)) {
            var key = data(4)
            if (baseMap.contains(key)) {
              var count: Int = baseMap.get(key).get
              baseMap.put(key, count + 1)
            } else {
              baseMap.put(key, 1)
            }
          }
        }
        for (elem <- baseMap) {
          baseMap.put(elem._1, elem._2 / (baseTime / reqTime).toInt)
        }


      }
    })

    for (elem <- reqMap) {
      var base = 0
      if (!baseMap.get(elem._1).isEmpty) {
        base = baseMap.get(elem._1).get
      }
      var d = (elem._2 - base)
      reqList += d
    }

    //T-TEST
    var pv = st.tTest(reqList)
    println(" 결과 ")
    println(path + "  :  " + pv)
    return pv


  }
}



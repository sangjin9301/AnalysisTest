package src

import java.io.{BufferedReader, FileReader}

import org.apache.commons.io.LineIterator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompareCallLog {
  def excute(path: String, reqTime: Double, baseTime: Double): Double = {
    var st = new StatisticsRule
    var reqMap_List = new ArrayBuffer[mutable.HashMap[String, Int]]
    var baseMap_List = new ArrayBuffer[mutable.HashMap[String, Int]]
    var CDR_List = new ArrayBuffer[String]
    var LineList = this.getData(path)
    var CDRArr = this.ArrayToLocation(LineList)
    var last = CDRArr.last.getDate

    //Map initialize
    for (i <- 0 to 6) {
      reqMap_List += new mutable.HashMap[String, Int]
      baseMap_List += new mutable.HashMap[String, Int]
    }
    //0으로 초기화
    CDRArr.foreach(loc => {
      reqMap_List(loc.getWeek - 1).put((loc.getX, loc.getY), 0)
      baseMap_List(loc.getWeek - 1).put((loc.getX, loc.getY), 0)
      if (!CDR_List.contains((loc.getX, loc.getY))) CDR_List += ((loc.getX, loc.getY))
    })

    //요일별(1~7)맵에 데이터를 삽입
    CDRArr.foreach(loc => {
      if (loc.getDate < last) {
        if (loc.getDate > last - reqTime) {
          var value = reqMap_List(loc.getWeek - 1).get((loc.getX, loc.getY)).get + 1
          reqMap_List(loc.getWeek - 1).put((loc.getX, loc.getY), value)
        }
      }

      if (loc.getDate < last - reqTime) {
        var value = baseMap_List(loc.getWeek - 1).get((loc.getX, loc.getY)).get + 1
        baseMap_List(loc.getWeek - 1).put((loc.getX, loc.getY), value)
      }
    })


    var result = new ArrayBuffer[Double]
    var resultValue: Double = 0
    for (i <- 0 to 6) {
      var target_Req_Map = reqMap_List(i)
      var target_Base_Map = baseMap_List(i)
      var reqList = new ArrayBuffer[Double]
      var baseList = new ArrayBuffer[Double]
      CDR_List.foreach(x => {
        try {
          target_Base_Map.put(x, (target_Base_Map.get(x).get / (baseTime / reqTime)).toInt)
          reqList += target_Req_Map.get(x).get
          baseList += target_Base_Map.get(x).get
        } catch {
          case e: NoSuchElementException => {}
        }
      })
      result += st.zTest(reqList, baseList)
    }
    var sum: Double = 0
    result.foreach(x => {
      sum += x
    })
    //평균을 반환
    resultValue = sum / result.size
    println(path + "  ::  " + resultValue)
    return resultValue
  }


  def getData(path: String): ArrayBuffer[Array[String]] = {
    var LineList = new ArrayBuffer[Array[String]]
    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(path))
    val iter = new LineIterator(bufReader)

    //csv파일에서 데이터 불러오기
    while (iter.hasNext) {
      LineList += iter.nextLine.split(",")
    }
    LineList.remove(0)
    return LineList
  }
}



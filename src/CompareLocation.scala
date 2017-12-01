package src

import java.io.{BufferedReader, FileReader}
import java.text.SimpleDateFormat
import java.util.Calendar

import org.apache.commons.io.LineIterator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompareLocation {

  def excute(path: String, reqTime: Double, baseTime: Double): (Boolean, Double) = {
    var st = new StatisticsRule
    var reqMap_List = new ArrayBuffer[mutable.HashMap[(Double, Double), Int]]
    var baseMap_List = new ArrayBuffer[mutable.HashMap[(Double, Double), Int]]
    var LineList = this.getData(path)
    var locationArr = this.ArrayToLocation(LineList)

    //Map initialize
    for (i <- 1 to 7) {
      reqMap_List(i) = new mutable.HashMap[(Double, Double), Int]
    }

    locationArr.foreach(loc => {
      var value = reqMap_List(loc.getWeek).get((loc.getX,loc.getY)).get+1
      reqMap_List(loc.getWeek).put((loc.getX,loc.getY),value)
    })

    //    mkRequest(locationArr, reqTime, reqMap_List)
    //    mkBase(locationArr, reqTime, baseTime, baseMap_List)

  }

  def mkRequest(locList: ArrayBuffer[Location], reqTime: Double, reqMap_List: ArrayBuffer[mutable.HashMap[(Double, Double), Int]]): Unit = {
    var lastDate = locList.last.getDate
    var term = lastDate - reqTime
    for (target <- locList) {
      if ((target.getDate < lastDate) && (target.getDate > term)) {
        var increase: Int = reqMap_List.get((target.getX, target.getY)).get + 1
        reqMap_List.put((target.getX, target.getY), increase)
      }
    }
  }

  def mkBase(locList: ArrayBuffer[Location], reqTime: Double, baseTime: Double, baseMap_List: ArrayBuffer[mutable.HashMap[(Double, Double), Int]]): Unit = {
    var lastDate = locList.last.getDate
    var term = lastDate - reqTime
    for (target <- locList) {
      if ((target.getDate < term)) {
      }
    }
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

  def ArrayToLocation(arr: ArrayBuffer[Array[String]]): ArrayBuffer[Location] = {
    var locationArr = new ArrayBuffer[Location]
    arr.foreach(elem => {
      var loc = new Location
      loc.setUser(elem(0))
      loc.setDate(elem(1).toDouble)
      loc.setTime(elem(2).toInt)
      loc.setWeek(elem(3).toInt)
      loc.setX(elem(5).toDouble / 100)
      loc.setY(elem(6).toDouble / 100)
      locationArr += loc
    })

    return locationArr
  }

  def getWeek(dateString: String): Int = {
    var format = "yyyy-MM-dd HH:mm"

    var df = new SimpleDateFormat(format)
    var date = df.parse(dateString)

    var cal = Calendar.getInstance()
    cal.setTime(date)
    var week = cal.get(Calendar.DAY_OF_WEEK)
    var weekString = ""

    return week
  }
}

}

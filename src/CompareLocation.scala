package src

import java.io.{BufferedReader, FileReader}
import java.text.SimpleDateFormat
import java.util.Calendar

import org.apache.commons.io.LineIterator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompareLocation {

  def excute(path: String, reqTime: Double, baseTime: Double): Double = {
    var st = new StatisticsRule
    var reqMap_List = new ArrayBuffer[mutable.HashMap[(Double, Double), Int]]
    var baseMap_List = new ArrayBuffer[mutable.HashMap[(Double, Double), Int]]
    var locationList = new ArrayBuffer[(Double,Double)]
    var LineList = this.getData(path)
    var locationArr = this.ArrayToLocation(LineList)
    var last = locationArr.last.getDate

    //Map initialize
    for (i <- 0 to 6) {
      reqMap_List += new mutable.HashMap[(Double, Double), Int]
      baseMap_List += new mutable.HashMap[(Double, Double), Int]
    }
    //0으로 초기화
    locationArr.foreach(loc => {
      reqMap_List(loc.getWeek-1).put((loc.getX,loc.getY), 0)
      baseMap_List(loc.getWeek-1).put((loc.getX,loc.getY), 0)
      if(!locationList.contains((loc.getX,loc.getY)))locationList += ((loc.getX,loc.getY))
    })

    //요일별(1~7)맵에 데이터를 삽입
    locationArr.foreach(loc => {
      if(loc.getDate<last){
        if(loc.getDate>last-reqTime){
          var value = reqMap_List(loc.getWeek-1).get((loc.getX,loc.getY)).get+1
          reqMap_List(loc.getWeek-1).put((loc.getX,loc.getY),value)
        }
      }

      if(loc.getDate<last-reqTime){
        var value = baseMap_List(loc.getWeek-1).get((loc.getX,loc.getY)).get+1
        baseMap_List(loc.getWeek-1).put((loc.getX,loc.getY),value)
      }
    })


    var result = new ArrayBuffer[Double]
    var resultValue:Double = 0
    for(i <- 0 to 6) {
      var target_Req_Map = reqMap_List(i)
      var target_Base_Map = baseMap_List(i)
      var reqList = new ArrayBuffer[Double]
      var baseList = new ArrayBuffer[Double]
      locationList.foreach(x => {
        try {
          target_Base_Map.put(x,(target_Base_Map.get(x).get / (baseTime / reqTime)).toInt)
          reqList +=  target_Req_Map.get(x).get
          baseList += target_Base_Map.get(x).get
        }catch {
          case e:NoSuchElementException => {}
        }
      })
      result += st.zTest(reqList,baseList)
    }
    var sum:Double = 0
    result.foreach(x=>{
      sum += x
    })
    //평균을 반환
    resultValue = sum/result.size
    println(path+"  ::  "+resultValue)
    return resultValue
  }

//  def mkRequest(locList: ArrayBuffer[Location], reqTime: Double, reqMap_List: ArrayBuffer[mutable.HashMap[(Double, Double), Int]]): Unit = {
//    var lastDate = locList.last.getDate
//    var term = lastDate - reqTime
//    for (target <- locList) {
//      if ((target.getDate < lastDate) && (target.getDate > term)) {
//        var increase: Int = reqMap_List.get((target.getX, target.getY)).get + 1
//        reqMap_List.put((target.getX, target.getY), increase)
//      }
//    }
//  }
//
//  def mkBase(locList: ArrayBuffer[Location], reqTime: Double, baseTime: Double, baseMap_List: ArrayBuffer[mutable.HashMap[(Double, Double), Int]]): Unit = {
//    var lastDate = locList.last.getDate
//    var term = lastDate - reqTime
//    for (target <- locList) {
//      if ((target.getDate < term)) {
//      }
//    }
//  }

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

      try {
        var loc = new Location
        loc.setUser(elem(0))
        loc.setDate(elem(1).toDouble)
        loc.setTime(elem(2).toInt)
        loc.setWeek(elem(3).toInt)
        loc.setX(elem(5).toDouble / 5000)
        loc.setY(elem(6).toDouble / 5000)
        locationArr += loc
      }catch{case e:ArrayIndexOutOfBoundsException => {}}

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


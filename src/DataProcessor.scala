package src

import java.io.{BufferedReader, File, FileReader, FileWriter}
import java.text.SimpleDateFormat
import org.apache.commons.io.LineIterator
import scala.collection.mutable.ArrayBuffer
import scala.util._

class DataProcessor {
  var cr = new CSVReader
  var locationPath:String = this.getClass.getResource(".").getPath.replace("src", "location")
  var callLogPath:String = this.getClass.getResource(".").getPath.replace("src", "calllog")

  def mkStolenLocationData(userList: ArrayBuffer[String]): Unit =
    {

      var bufReader: BufferedReader = null;
      //한 명의 데이터에 대해
      userList.foreach((id: String) => {
        var readLocationPath = locationPath + id + "/original/" + id + ".csv"
        bufReader = new BufferedReader(new FileReader(readLocationPath))
        val iter = new LineIterator(bufReader)
        var LineList = new ArrayBuffer[Array[String]]
        var line: String = ""

        var sb = new StringBuilder

        while (iter.hasNext) {
          //새로 쓰일 문자열을 채움.
          line = iter.nextLine
          sb.append(line)
          sb.append("\n")
          LineList += line.split(",")
        }
        var lastTime = LineList.last(1)

        writerStolenLocationData(id, userList, lastTime, 21600, sb)//6시간
        writerStolenLocationData(id, userList, lastTime, 86400, sb)//1일
        writerStolenLocationData(id, userList, lastTime, 604800, sb)//1주일

      })

    }

  def mkStolenCallLogData(userList: ArrayBuffer[String]): Unit =
  {

    var bufReader: BufferedReader = null;
    //한 명의 데이터에 대해
    userList.foreach((id: String) => {
      var readCallLogPath = callLogPath + id + "/original/" + id + ".csv"
      bufReader = new BufferedReader(new FileReader(readCallLogPath))
      val iter = new LineIterator(bufReader)
      var LineList = new ArrayBuffer[Array[String]]
      var line: String = ""

      var sb = new StringBuilder

      while (iter.hasNext) {
        //새로 쓰일 문자열을 채움.
        line = iter.nextLine
        sb.append(line)
        sb.append("\n")
        LineList += line.split(",")
      }
      var lastTime = LineList.last(1)

      writerStolenCallLogData(id, userList, lastTime, 21600, sb)//6시간
      writerStolenCallLogData(id, userList, lastTime, 86400, sb)//1일
      writerStolenCallLogData(id, userList, lastTime, 604800, sb)//1주일

    })

  }

  def writerStolenLocationData(id: String, userList: ArrayBuffer[String], lastTime: String, reqTime:Int,  sb: StringBuilder): Unit =
    {
      var someone = new ArrayBuffer[String]
      for(i <- 0 to 4){
        someone += userList(Random.nextInt(userList.size))
      }

      someone.foreach((other: String) => {
        var otherArray = cr.readLocationData(other)
        //타겟의 마지막 활동 시간보다 큰 데이터인지 확인.
        if (otherArray.last(1) > lastTime) {
          var newsb = new StringBuilder
          otherArray.remove(0)
          otherArray.foreach((elm: Array[String]) => {
            //타겟의 마지막 시간 뒤에 다른 사람 그 시간 후의 데이터를 붙임.
            if (elm(1).toInt > lastTime.toInt) {
              if (elm(1).toInt < (lastTime.toInt + reqTime)) {
                newsb.append(elm(0))
                newsb.append(",")
                newsb.append(elm(1))
                newsb.append(",")
                newsb.append(elm(2))
                newsb.append(",")
                newsb.append(elm(3))
                newsb.append(",")
                newsb.append(elm(4))
                newsb.append(",")
                newsb.append(elm(5))
                newsb.append("\n")
              }

            }
          })

          if (!newsb.isEmpty) {
            var strbd = new StringBuilder
            strbd.append(sb.mkString)
            strbd.append(newsb.mkString)

            //스틸 데이터를 새로 저장.
            var file = new File(locationPath + id + "/"+reqTime.toString+"secStolenBy");
            if (!file.exists) {
              file.mkdirs;
            }
            var writePath = locationPath + id + "/"+reqTime.toString+"secStolenBy/" + other + ".csv"
            var writer = new FileWriter(writePath)
            println("write steel data")
            writer.write(strbd.mkString)
            writer.close()
          }

        }
      })
    }

  def writerStolenCallLogData(id: String, userList: ArrayBuffer[String], lastTime: String, reqTime:Int,  sb: StringBuilder): Unit =
  {
    var someone = new ArrayBuffer[String]
    for(i <- 0 to 4){
      someone += userList(Random.nextInt(userList.size))
    }

    someone.foreach((other: String) => {
      var otherArray = cr.readCDRData(other)
      //타겟의 마지막 활동 시간보다 큰 데이터인지 확인.
      if (otherArray.last(1) > lastTime) {
        var newsb = new StringBuilder
        otherArray.remove(0)
        otherArray.foreach((elm: Array[String]) => {
          //타겟의 마지막 시간 뒤에 다른 사람 그 시간 후의 데이터를 붙임.
          if (elm(1).toInt > lastTime.toInt) {
            if (elm(1).toInt < (lastTime.toInt + reqTime)) {
              newsb.append(elm(0))
              newsb.append(",")
              newsb.append(elm(1))
              newsb.append(",")
              newsb.append(elm(2))
              newsb.append(",")
              newsb.append(elm(3))
              newsb.append(",")
              newsb.append(elm(4))
              newsb.append("\n")
            }

          }
        })

        if (!newsb.isEmpty) {
          var strbd = new StringBuilder
          strbd.append(sb.mkString)
          strbd.append(newsb.mkString)

          //스틸 데이터를 새로 저장.
          var file = new File(callLogPath + id + "/"+reqTime.toString+"secStolenBy");
          if (!file.exists) {
            file.mkdirs;
          }
          var writePath = callLogPath + id + "/"+reqTime.toString+"secStolenBy/" + other + ".csv"
          var writer = new FileWriter(writePath)
          println("write steel data")
          writer.write(strbd.mkString)
          writer.close()
        }

      }
    })
  }

  def date2Timestamp(date_str: String, format: String): String =
    {
      var sdf: SimpleDateFormat = new SimpleDateFormat(format)
      return String.valueOf(sdf.parse(date_str).getTime / 1000)
    }

  def timestamp: String =
    {
      var time: Long = System.currentTimeMillis()
      var t: String = String.valueOf(time / 1000)
      return t
    }
}
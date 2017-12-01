package src

import java.io._
import java.text.SimpleDateFormat
import java.util.Calendar
import org.apache.commons.io.LineIterator
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

class CSVReader {

  var locationPath: String = this.getClass.getResource(".").getPath.replace("src", "location")
  var callLogPath: String = this.getClass.getResource(".").getPath.replace("src", "calllog")
  val ID_LIST = this.getClass.getResource(".").getPath.replace("src", "location") + "UserList.csv"
  var user = new ArrayBuffer[String]
  getList(ID_LIST)

  def getList(path: String): Unit = {
    var bufReader: BufferedReader = null;
    var line = ""
    var csvSplitBy = ","
    try {

      bufReader = new BufferedReader(new FileReader(path))
      val iter = new LineIterator(bufReader)
      while (iter.hasNext) {
        line = iter.nextLine
        var id = line.split(csvSplitBy)
        this.user += id(0)
      }

    } catch {
      case e: IOException => {
        e.printStackTrace()
      }
    }
  }

  def getUser: ArrayBuffer[String] = {
    return this.user
  }

  def readLocationData(user: String): ArrayBuffer[Array[String]] = {
    var bufReader: BufferedReader = null
    var readPath = locationPath + user + "/original/" + user + ".csv"
    bufReader = new BufferedReader(new FileReader(readPath))
    val iter = new LineIterator(bufReader)

    var LineList = new ArrayBuffer[Array[String]]
    while (iter.hasNext) {
      LineList += iter.nextLine.split(",")
    }
    return LineList
  }

  def readCDRData(user: String): ArrayBuffer[Array[String]] = {
    var bufReader: BufferedReader = null;
    var readPath = callLogPath + user + "/original/" + user + ".csv"
    bufReader = new BufferedReader(new FileReader(readPath))
    val iter = new LineIterator(bufReader)

    var LineList = new ArrayBuffer[Array[String]]
    while (iter.hasNext) {
      LineList += iter.nextLine.split(",")
    }
    return LineList
  }

  def read(path: String): Unit = {
    var bufReader: BufferedReader = null;
    var line = ""
    var csvSplitBy = ","

    try {

      bufReader = new BufferedReader(new FileReader(path))

      val iter = new LineIterator(bufReader)

      var LineList = new ArrayBuffer[String]
      while (iter.hasNext) {
        LineList += iter.nextLine
      }

      user.foreach((id: String) => {
        if (path.contains("location")) writeLocationByUser(id, LineList.toArray)
        if (path.contains("calllog")) writeCDRByUser(id, LineList.toArray)

      })

      bufReader.close
    } catch {
      case e: FileNotFoundException => {
        e.printStackTrace
      }
      case e: IOException => {
        e.printStackTrace
      }
      case e: ArrayIndexOutOfBoundsException => {
        e.printStackTrace
      }
    } finally {
      if (bufReader != null) {
        try {
          bufReader.close
        } catch {
          case e: IOException => {
            e.printStackTrace
          }
        }
      }
    }
  }

  def writeLocationByUser(user: String, data: Array[String]): Unit = {
    var dp = new DataProcessor
    var fileName = locationPath + user + "/original/" + user + ".csv"
    var file = new File(locationPath + user + "/original")
    if (!file.exists) {
      file.mkdirs()
    }
    var writer = new FileWriter(fileName)
    var line = ""
    var csvSplitBy = ","

    var sb = new StringBuilder
    sb.append("ID")
    sb.append(",")
    sb.append("DATE")
    sb.append(",")
    sb.append("TIME")
    sb.append(",")
    sb.append("WEEK")
    sb.append(",")
    sb.append("ACCURACY")
    sb.append(",")
    sb.append("X")
    sb.append(",")
    sb.append("Y")
    sb.append("\n")

    for (i <- 1 to data.length - 1) {
      breakable {
        line = data(i)
        if (line.contains("Inf")) break

        //use comma as separator
        var location = line.split(csvSplitBy)
        var time = location(1).split(" ")(1).split(":")(0)
        var ts = dp.date2Timestamp(location(1), "yyyy-MM-dd HH:mm")
        var week = getWeek(location(1))
        if (location(0).equals(user)) {
          sb.append(user)
          sb.append(",")
          sb.append(ts)
          sb.append(",")
          sb.append(time)
          sb.append(",")
          sb.append(week)
          sb.append(",")
          sb.append(location(2))
          sb.append(",")
          sb.append(location(3))
          sb.append(",")
          sb.append(location(4))
          sb.append("\n")
          println("location   ...  " + user)
        }
      }
    }
    writer.write(sb.mkString)
    writer.close()

  }

  def writeCDRByUser(user: String, data: Array[String]): Unit = {
    var dp = new DataProcessor
    var fileName = callLogPath + user + "/original/" + user + ".csv"
    var file = new File(callLogPath + user + "/original")
    if (!file.exists) {
      file.mkdirs();
    }
    var writer = new FileWriter(fileName)
    var line = ""
    var csvSplitBy = ","

    var sb = new StringBuilder
    sb.append("ID")
    sb.append(",")
    sb.append("DATE")
    sb.append(",")
    sb.append("TIME")
    sb.append(",")
    sb.append("WEEK")
    sb.append(",")
    sb.append("TYPE")
    sb.append(",")
    sb.append("TARGET")
    sb.append("\n")

    for (i <- 1 to data.length - 1) {
      breakable {
        line = data(i)
        if (line.contains("Inf")) break

        //use comma as separator
        var log = line.split(csvSplitBy)
        var id = log(0).replace("\"", "")
        var time = log(2).split(" ")(1).split(":")(0)
        var ts = dp.date2Timestamp(log(2), "yyyy-MM-dd HH:mm")
        var week = getWeek(log(2))

        println("call log  ...  " + user + " : " + id)
        if (user.equals(id)) {
          sb.append(user)
          sb.append(",")
          sb.append(ts)
          sb.append(",")
          sb.append(time)
          sb.append(",")
          sb.append(week)
          sb.append(",")
          sb.append(log(3))
          sb.append(",")
          sb.append(log(5))
          sb.append("\n")
          println("call log ###########################################################")
        }
      }
    }
    writer.write(sb.mkString)
    writer.close()

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
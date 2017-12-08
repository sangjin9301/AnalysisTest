package src

import java.io.{File, FileWriter}

import org.apache.commons.math3.distribution.NormalDistribution

import scala.collection.mutable.ArrayBuffer

object Test {

  def locationEvaluation: Unit = {
    println("Rule Excute")
    var fileName = this.getClass.getResource(".").getPath.replace("src", "Location_Day&1Month_Rule.csv")
    var writer = new FileWriter(fileName)


    var oneDay = 86400
    var cl = new CompareLocation

    var cr = new CSVReader
    var userList = cr.user
    var sb = new StringBuilder
    var cal = new StringBuilder
    cal.append("user")
    cal.append(",")
    cal.append("SteelBy")
    cal.append(",")
    cal.append("result")
    cal.append("\n")
    writer.write(cal.mkString)


    userList.foreach(user => {
      var path = this.getClass.getResource(".").getPath.replace("src", "location") + "/" + user + "/original/" + user + ".csv"

      sb.append(user)
      sb.append(",")
      sb.append(user)
      sb.append(",")
      sb.append(cl.excute((path), oneDay, oneDay * 30))
      sb.append("\n")

      try {
        path = this.getClass.getResource(".").getPath.replace("src", "location") + "/" + user + "/604800secStolenBy/"
        var dir = new File(path)
        dir.listFiles().foreach(f => {
          sb.append(user)
          sb.append(",")
          sb.append(f.getName)
          sb.append(",")
          sb.append(cl.excute((path + f.getName), oneDay, oneDay * 30))
          sb.append("\n")
        })

      } catch {
        case e: NullPointerException => {
          writer.write(sb.mkString)
          writer.flush
        }
      }
    }


    )
    writer.write(sb.mkString)
    writer.close()
  }


  def cdrEvaluation: Unit = {
    var fileName = this.getClass.getResource(".").getPath.replace("src", "CDR_Day&1Month_Rule.csv")
    var writer = new FileWriter(fileName)


    var oneDay = 86400
    var cc = new CompareCallLog

    var cr = new CSVReader
    var userList = cr.user
    var sb = new StringBuilder
    var cal = new StringBuilder
    cal.append("user")
    cal.append(",")
    cal.append("SteelBy")
    cal.append(",")
    cal.append("result")
    cal.append("\n")
    writer.write(cal.mkString)


    userList.foreach(user => {
      var path = this.getClass.getResource(".").getPath.replace("src", "calllog") + "/" + user + "/original/" + user + ".csv"

      sb.append(user)
      sb.append(",")
      sb.append(user)
      sb.append(",")
      sb.append(cc.excute((path), oneDay, oneDay * 30))
      sb.append("\n")

      try {
        path = this.getClass.getResource(".").getPath.replace("src", "calllog") + "/" + user + "/604800secStolenBy/"
        var dir = new File(path)
        dir.listFiles().foreach(f => {
          sb.append(user)
          sb.append(",")
          sb.append(f.getName)
          sb.append(",")
          sb.append(cc.excute((path + f.getName), oneDay, oneDay * 30))
          sb.append("\n")
        })
      } catch {
        case e: NullPointerException => {
          writer.write(sb.mkString)
          writer.flush
        }
      }
    })
    writer.write(sb.mkString)
    writer.flush
  }


  def main(args: Array[String]): Unit = {
//            cdrEvaluation
            locationEvaluation
//    var locationPath: String = this.getClass.getResource(".").getPath.replace("src", "location")
//    var callLogPath: String = this.getClass.getResource(".").getPath.replace("src", "calllog")
//    println(locationPath)
//    println(callLogPath)
//    //
//    var cr = new CSVReader
//    var dp = new DataProcessor
//    //    cr.read(callLogPath+"CallLog.csv")
//    //    dp.mkStolenCallLogData(cr.user)
//    dp.mkStolenLocationData(cr.user)
  }
}

//        var locationPath:String = this.getClass.getResource(".").getPath.replace("src", "location")
//        var callLogPath:String = this.getClass.getResource(".").getPath.replace("src", "calllog")
//        println(locationPath)
//        println(callLogPath)
//
//        var cr = new CSVReader
//        var dp = new DataProcessor
//
//        cr.read(callLogPath+"CallLog.csv")
//        cr.read(locationPath+"Location.csv")
//
//        dp.mkStolenLocationData(cr.user)
//        dp.mkStolenCallLogData(cr.user)

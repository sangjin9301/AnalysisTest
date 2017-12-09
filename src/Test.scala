package src

import java.io.{File, FileWriter}

import org.apache.commons.math3.distribution.NormalDistribution

import scala.collection.mutable.ArrayBuffer

object Test {

  def locationEvaluation(req: Double, base: Double, write: String): Unit = {
    println("Rule Excute")
    var fileName = this.getClass.getResource(".").getPath.replace("src", write)
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
      var result = false
      if (cl.excute((path), oneDay * req, oneDay * base) >= 0.5) result = true
      sb.append(user)
      sb.append(",")
      sb.append(user)
      sb.append(",")
      sb.append(result)
      sb.append("\n")

      try {
        path = this.getClass.getResource(".").getPath.replace("src", "location") + "/" + user + "/604800secStolenBy/"
        var dir = new File(path)
        dir.listFiles().foreach(f => {
          var result = false
          if (cl.excute((path + f.getName), oneDay * req, oneDay * base) >= 0.5) result = true
          sb.append(user)
          sb.append(",")
          sb.append(f.getName)
          sb.append(",")
          sb.append(result)
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


  def cdrEvaluation(req: Double, base: Double, write: String): Unit = {
    var fileName = this.getClass.getResource(".").getPath.replace("src", write)
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
      var result = false
      if (cc.excute((path), oneDay * req, oneDay * base) >= 0.5) result = true
      sb.append(user)
      sb.append(",")
      sb.append(user)
      sb.append(",")
      sb.append(result)
      sb.append("\n")

      try {
        path = this.getClass.getResource(".").getPath.replace("src", "calllog") + "/" + user + "/604800secStolenBy/"
        var dir = new File(path)
        dir.listFiles().foreach(f => {
          var result = false
          if (cc.excute((path + f.getName), oneDay * req, oneDay * base) >= 0.5) result = true
          sb.append(user)
          sb.append(",")
          sb.append(f.getName)
          sb.append(",")
          sb.append(result)
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

  def summaryTest: Unit = {
    var mksummary = new MakeSummary
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/CDR_Day&1Month_Rule.csv", "CDR_Day&1Month_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/Location_Day&1Month_Rule.csv", "Location_Day&1Month_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/CDR_1Hour&1Day_Rule.csv", "CDR_1Hour&1Day_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/Location_1Hour&1Day_Rule.csv", "Location_1Hour&1Day_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/CDR_3Hour&3Day_Rule.csv", "CDR_3Hour&3Day_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/Location_3Hour&3Day_Rule.csv", "Location_3Hour&3Day_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/CDR_6Hour&7Day_Rule.csv", "CDR_6Hour&7Day_Rule_PrecisionRecall")
    mksummary.writeSummary("D:/Context_Aware_Authentication/AnalysisTest/out/production/AnalysisTest/Location_6Hour&7Day_Rule.csv", "Location_6Hour&7Day_Rule_PrecisionRecall")
  }

  def mkDataSet: Unit = {
    var locationPath: String = this.getClass.getResource(".").getPath.replace("src", "location")
    var callLogPath: String = this.getClass.getResource(".").getPath.replace("src", "calllog")
    println(locationPath)
    println(callLogPath)

    var cr = new CSVReader
    var dp = new DataProcessor
    cr.read(callLogPath + "CallLog.csv")
    cr.read(locationPath + "Location.csv")
    dp.mkStolenCallLogData(cr.user)
    dp.mkStolenLocationData(cr.user)
  }

  def main(args: Array[String]): Unit = {
    cdrEvaluation(0.417, 1, "CDR_1Hour&1Day_Rule.csv") // 1시간-1일
    locationEvaluation(0.417, 1, "Location_1Hour&1Day_Rule.csv")
    cdrEvaluation(0.125, 3, "CDR_3Hour&3Day_Rule.csv") // 3시간-3일
    locationEvaluation(0.125, 3, "Location_3Hour&3Day_Rule.csv")
    cdrEvaluation(0.25, 7, "CDR_6Hour&7Day_Rule.csv") // 6시간-1주일
    locationEvaluation(0.25, 7, "Location_6Hour&7Day_Rule.csv")
    summaryTest
  }
}


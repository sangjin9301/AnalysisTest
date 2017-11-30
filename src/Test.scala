package src

import java.io.{File, FileWriter}

object Test {

  def locationEvaluation: Unit = {
    var fileName = this.getClass.getResource(".").getPath.replace("src", "Steel1Week_1Day&30DayRule.csv")
    var writer = new FileWriter(fileName)


    var oneDay = 86400
    var cl = new CompareLocation

    var cr = new CSVReader
    var userList = cr.user
    var sb = new StringBuilder
    sb.append("user")
    sb.append(",")
    sb.append("SteelBy")
    sb.append(",")
    sb.append("result")
    sb.append(",")
    sb.append("p-value")
    sb.append("\n")



    userList.foreach(user => {
      var path = this.getClass.getResource(".").getPath.replace("src", "location") + "/" + user + "/original/" + user + ".csv"

      sb.append(user)
      sb.append(",")
      sb.append(user)
      sb.append(",")
      sb.append(cl.excute((path), oneDay, oneDay * 30)._1)
      sb.append(",")
      sb.append(cl.excute((path), oneDay, oneDay * 30)._2)
      sb.append("\n")

      try {
        path = this.getClass.getResource(".").getPath.replace("src", "location") + "/" + user + "/604800secStolenBy/"
        var dir = new File(path)
        dir.listFiles().foreach(f => {
          sb.append(user)
          sb.append(",")
          sb.append(f.getName)
          sb.append(",")
          sb.append(cl.excute((path + f.getName), oneDay, oneDay * 30)._1)
          sb.append(",")
          sb.append(cl.excute((path + f.getName), oneDay, oneDay * 30)._2)
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
    writer.close()
  }



  def cdrEvaluation:Unit=
  {
    var fileName = this.getClass.getResource(".").getPath.replace("src", "Steel1Week_1Day&30DayRule02.csv")
    var writer = new FileWriter(fileName)


    var oneDay = 86400
    var cc = new CompareCallLog

    var cr = new CSVReader
    var userList = cr.user
    var sb = new StringBuilder
    sb.append("user")
    sb.append(",")
    sb.append("SteelBy")
    sb.append(",")
    sb.append("result")
    sb.append(",")
    sb.append("p-value")
    sb.append("\n")



    userList.foreach(user => {
      var path = this.getClass.getResource(".").getPath.replace("src", "calllog") + "/" + user + "/original/" + user + ".csv"

      sb.append(user)
      sb.append(",")
      sb.append(user)
      sb.append(",")
      sb.append(cc.excute((path), oneDay, oneDay * 30)._1)
      sb.append(",")
      sb.append(cc.excute((path), oneDay, oneDay * 30)._2)
      sb.append("\n")

      try {
        path = this.getClass.getResource(".").getPath.replace("src", "calllog") + "/" + user + "/604800secStolenBy/"
        var dir = new File(path)
        dir.listFiles().foreach(f => {
          sb.append(user)
          sb.append(",")
          sb.append(f.getName)
          sb.append(",")
          sb.append(cc.excute((path + f.getName), oneDay, oneDay * 30)._1)
          sb.append(",")
          sb.append(cc.excute((path + f.getName), oneDay, oneDay * 30)._2)
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
//    cdrEvaluation
//    locationEvaluation
        var locationPath:String = this.getClass.getResource(".").getPath.replace("src", "location")
        var callLogPath:String = this.getClass.getResource(".").getPath.replace("src", "calllog")
        println(locationPath)
        println(callLogPath)

        var cr = new CSVReader
        var dp = new DataProcessor

        cr.read(callLogPath+"CallLog.csv")
        cr.read(locationPath+"Location.csv")

        dp.mkStolenLocationData(cr.user)
        dp.mkStolenCallLogData(cr.user)
  }
}



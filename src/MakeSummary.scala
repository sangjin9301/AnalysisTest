package src

import java.io.{BufferedReader, File, FileReader, FileWriter}

import org.apache.commons.io.LineIterator

import scala.collection.mutable.ArrayBuffer

/**
  * Created by SANGJIN-NAM on 2017-12-09.
  */
class MakeSummary {

  def writeSummary(readPath: String, writeFName: String): Unit = {
    val writeDirPath = this.getClass.getResource(".").getPath.replace("src", "") //directory
    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(readPath))
    val iter = new LineIterator(bufReader)

    var LineList = new ArrayBuffer[Array[String]]
    while (iter.hasNext) {
      LineList += iter.nextLine.split(",")
    }
    LineList.remove(0)


    var predict_list = new ArrayBuffer[Boolean]
    var actual_list = new ArrayBuffer[Boolean]
    var tp_list = new ArrayBuffer[Boolean]
    var fn_list = new ArrayBuffer[Boolean]
    var fp_list = new ArrayBuffer[Boolean]
    var tn_list = new ArrayBuffer[Boolean]

    LineList.foreach(line => {
      var user1 = line(0)
      var user2 = line(1)
      var predict = line(2).toBoolean
      var actual = getActual(user1, user2)

      predict_list += predict
      actual_list += actual
      fn_list += getFalseNegative(predict, actual)
      fp_list += getFalsePositive(predict, actual)
      tn_list += getTrueNegative(predict, actual)
      tp_list += getTruePositive(predict, actual)

    })

    var fn_list_count: Double = getTrueCount(fn_list)
    var fp_list_count: Double = getTrueCount(fp_list)
    var tn_list_count: Double = getTrueCount(tn_list)
    var tp_list_count: Double = getTrueCount(tp_list)

    var sb = new StringBuilder
    sb.append("Predicted")
    sb.append(",")
    sb.append("Actual")
    sb.append(",")
    sb.append("TruePositive")
    sb.append(",")
    sb.append("FalseNegative")
    sb.append(",")
    sb.append("FalsePositive")
    sb.append(",")
    sb.append("TrueNegative")
    sb.append("\n")

    for (i <- 0 to LineList.size - 1) {
      sb.append(predict_list(i))
      sb.append(",")
      sb.append(actual_list(i))
      sb.append(",")
      sb.append(tp_list(i))
      sb.append(",")
      sb.append(fn_list(i))
      sb.append(",")
      sb.append(fp_list(i))
      sb.append(",")
      sb.append(tn_list(i))
      sb.append("\n")
    }

    sb.append("\n")
    sb.append("Precision")
    sb.append(",")
    sb.append("Recall")
    sb.append(",")
    sb.append("Accuracy")
    sb.append(",")
    sb.append("F-measure")
    sb.append("\n")

    var Precision = tp_list_count / (tp_list_count + fp_list_count)
    var Recall = tp_list_count / (tp_list_count + fn_list_count)
    var Accuracy = (tp_list_count + tn_list_count) / LineList.size
    var F_Measure = (2 * Precision * Recall) / (Precision + Recall)
    sb.append(Precision)
    sb.append(",")
    sb.append(Recall)
    sb.append(",")
    sb.append(Accuracy)
    sb.append(",")
    sb.append(F_Measure)
    sb.append("\n")

    var file = new File(writeDirPath)
    if (!file.exists) {
      file.mkdirs
    }
    var writePath: String = writeDirPath + "/" + writeFName + ".csv"
    var writer = new FileWriter(writePath)
    writer.write(sb.mkString)
    writer.close()
  }


  def getActual(user1: String, user2: String): Boolean = {
    if (user1.equals(user2)) {
      return true
    }
    else return false
  }

  def getTruePositive(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(true)) {
      if (actu.equals(true)) {
        return true
      } else return false
    } else return false
  }

  def getFalseNegative(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(false)) {
      if (actu.equals(true)) {
        return true
      } else return false
    } else return false
  }

  def getFalsePositive(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(true)) {
      if (actu.equals(false)) {
        return true
      } else return false
    } else return false
  }

  def getTrueNegative(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(false)) {
      if (actu.equals(false)) {
        return true
      } else return false
    } else return false
  }

  def getTrueCount(column: ArrayBuffer[Boolean]): Double = {
    var count: Int = 0
    column.foreach(x => {
      if (x.equals(true)) count += 1
    })
    return count.toDouble
  }

}

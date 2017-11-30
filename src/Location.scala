package src

class Location {
  var user:String = ""
  var date:String = ""
  var x:String = ""
  var y:String = ""
  
  def setUser(user:String):Unit=
  {
    this.user = user
  }
  
  def setDate(date:String):Unit=
  {
    this.date = date
  }
  
  def setX(x:String):Unit=
  {
    this.x = x
  }
  
  def setY(y:String):Unit=
  {
    this.y = y
  }
  
  def getUser:String=
  {
    return this.user
  }
  
  def getDate:String=
  {
    return this.date
  }
  
  def getX:String=
  {
    return this.x
  }
  
  def getY:String=
  {
    return this.y
  }
}
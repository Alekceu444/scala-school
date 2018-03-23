package main.scala.lectures.l5

class User private[l5](val login:String, val pass:String){
  private[l5] val authorizedUser=Map("a"->(12,"ds"),"b"->(1234,"df"),"c"->(123456,"de"))
  def services(user:User):Unit={
    if (user.isInstanceOf[LoggedUser]) {
      val t=new excp
      t.PrintUserAlreadyAuthorizedException()
    }
    else if ((authorizedUser.contains(user.login) && authorizedUser(user.login)._1==user.pass)) {
      val loggedUser = new LoggedUser(user)
    }
    else{
      throw new IllegalArgumentException("No such user in base")
    }
  }
}
case class excp() extends Exception {
  def UserAlreadyAuthorizedException(): Int =
    throw new Exception("Alreade Authorized")
  def PrintUserAlreadyAuthorizedException() = {
    print(UserAlreadyAuthorizedException())
  }

}
class LoggedUser private[l5](user:User) extends User(user.login,user.pass){
  private val role=authorizedUser(user.login)._2
}


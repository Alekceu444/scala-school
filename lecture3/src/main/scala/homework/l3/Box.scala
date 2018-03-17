package homework.l3

import homework.l3

sealed trait Box
case class PlayStationBox() extends Box
case class GuitarBox() extends Box
case class EaselBox() extends Box
case class BasicBox() extends Box {
  val size: Int = 4
}
case class BigBox() extends Box {
  val size: Int = 10
}

trait Stuff
case class PlayStation() extends Stuff
case class Guitar() extends Stuff
case class TV(size: Int) extends Stuff
case class Easel() extends Stuff
case class Book() extends Stuff
case class Cat() extends Stuff
case class Uculele() extends Stuff
case class Dish() extends Stuff
case class Shoes() extends Stuff

object BoxPlan {
  def plan(stuff: Seq[Stuff]): Seq[Box] = {
    var boxes=scala.collection.mutable.ListBuffer.empty[Box]
    var basicbox=4
    var bigbox=10
    stuff.foreach{
      x=>{
        x match {
          case _:PlayStation=>boxes += PlayStationBox()
          case _:Guitar=>boxes += GuitarBox()
          case _:Easel=>boxes += EaselBox()
          case tv:TV if (4<tv.size && tv.size<=10 && bigbox>tv.size) ||
            (tv.size<bigbox && bigbox!=10)=>bigbox-=tv.size
          case tv:TV if 10>=tv.size && tv.size>4 && tv.size>bigbox=> {
            boxes += BigBox()
            bigbox= 10-tv.size
          }
          case tv:TV if basicbox>tv.size =>basicbox-=tv.size
          case tv:TV if basicbox<tv.size && tv.size<=4 => {
            boxes += BasicBox()
            basicbox= 4-tv.size
          }
          case tv:TV if tv.size>10 => throw new Exception("too big tv")
          case stuff:Stuff if 1<bigbox && bigbox!=10=>bigbox-=1
          case stuff:Stuff if basicbox>0 => basicbox-=1
          case stuff:Stuff if basicbox==0  =>{
            boxes += BasicBox()
            basicbox=3
          }
        }
      }
    }
    if (basicbox!=4) {boxes += BasicBox()}
    if (bigbox!=10) {boxes += BigBox()}
    boxes.toList
  }
}
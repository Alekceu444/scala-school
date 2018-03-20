package lectures.l4

object Collection2 extends App {

  sealed trait Transport

  case object Auto extends Transport

  case class Track(weight: Int) extends Transport

  // Реализовать функцию, выбирающую из списка только легковые машины и грузовики с весом меньше максимального
  // Релизовать тремя способами
  // - collect
  // - fold*
  // - map/flatMap
  // - filter

  def collectorA(seq: Seq[Transport], maxWeight: Int): Seq[Transport] = seq.collect{
    case track:Track if track.weight<maxWeight=>track
    case auto if auto.equals(Auto) => auto
  }

  def collectorB(seq: Seq[Transport], maxWeight: Int): Seq[Transport] = seq.foldLeft(Seq[Transport]()) { (m: Seq[Transport], n: Transport) =>
    n match {
      case df if df.equals(Auto)=> m:+df
      case fg:Track if fg.weight<maxWeight=>m:+fg
      case _ => m
    }
  }

  def collectorC(seq: Seq[Transport], maxWeight: Int): Seq[Transport] = seq.filter((i:Transport)=>i.equals(Auto)
    || i.asInstanceOf[Track].weight<maxWeight)


  val transports  = Seq(
    Auto, Auto, Auto, Track(100), Auto, Track(150), Track(110), Track(200)
  )

  println(collectorA(transports, 120)) // => Auto, Auto, Auto, Track(100), Auto, Track(110)
  println(collectorB(transports, 120)) // => Auto, Auto, Auto, Track(100), Auto, Track(110)
  println(collectorC(transports, 120)) // => Auto, Auto, Auto, Track(100), Auto, Track(110)

}

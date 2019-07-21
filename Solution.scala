object Solution {
    
    case class TimeEvent(time: Int, netInt: Int)
    
    def genTimeLine(intervals: Array[Array[Int]]): Seq[TimeEvent] = {
        val hm = scala.collection.mutable.Map.empty[Int, Int]
        
        intervals.foreach { i =>
            hm.update(i.head, hm.get(i.head).getOrElse(0) + 1)
            hm.update(i.last, hm.get(i.last).getOrElse(0) - 1)
        }
        
        hm.map({ case (k, v) => TimeEvent(k, v) }).toSeq.sortBy(_.time)
    }
    
    def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
        var results = Array.empty[Array[Int]]
        var start = 0
        var activeIntervals = 0
        def noActiveIntervals: Boolean = activeIntervals == 0
        
        genTimeLine(intervals).foreach { timeEvent =>
            if (noActiveIntervals) start = timeEvent.time
            activeIntervals += timeEvent.netInt
            if (noActiveIntervals) results = results :+ Array(start, timeEvent.time)
        }
        
        results
    }
   
}
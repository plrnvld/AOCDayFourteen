import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import java.util.Calendar
import scala.runtime.LongRef

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val start = lines(0)

        val polymer = start.toCharArray().toList
        val rules = lines.drop(2).map(line => (line(0), line(1)) -> line(6)).toMap
        val max_generation = 10
        val cache_level = 4
        
        countPolymer(polymer, max_generation, cache_level, rules).print();
    }

    def countPolymer(polymer: List[Char], maxGeneration: Int, cacheLevel: Int, rules: Map[(Char, Char), Char]): CharCounter = {
        var counter = new CharCounter()
        var safeChar = polymer(0);
        counter.count(safeChar)
      
        for (c <- polymer.drop(1)) {
            
            var levels = new Stack[(Char, Int)]()
            levels.prepend((c, 0))
            println(s"New round ${Calendar.getInstance().getTime()}")

            while (levels.size > 0) {
                var level = levels.head._2
                                
                while (level < maxGeneration) {
                    val headChar = levels.head._1

                    level = level + 1
                    levels.update(0, (headChar, level))

                    val remaining = maxGeneration - level

                    if (remaining == cacheLevel) {
                        // Use cache
                    }

                    val matchResult = rules.get((safeChar, headChar))
                    matchResult.foreach(newChar => levels.prepend((newChar, level)))
                }
                
                // level == max_generation
                safeChar = levels.pop()._1
                counter.count(safeChar)                           
            }
        }

        counter
    }

    class CharCounter {
        var counter: HashMap[Char, Long] = new HashMap()
        
        def count(c: Char): Unit = {
            upsert(c, v => v + 1)
        }

        def uncount(c: Char): Unit = {            
            upsert(c, v => v - 1)
        }

        def upsert(c: Char, update: Long => Long) {
            if (this.counter.contains(c)) {
                val oldValue = this.counter(c);
                this.counter(c) = update(oldValue)
            } else {
                val oldValue = 0
                this.counter += (c -> update(oldValue))
            }
        }

        def addCounter(other: CharCounter) {
            for (kv <- other.counter) {
                upsert(kv._1, v => v + kv._2)
            }
        }

        def print(): Unit = {
            val minItem = counter.minBy(_._2);
            val maxItem = counter.maxBy(_._2);

            println(s"Counter: ${counter}, min ${minItem}, max ${maxItem}, res = ${maxItem._2 - minItem._2}")
        }
    }
}

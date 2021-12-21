import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import java.util.Calendar

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val start = lines(0)

        var polymer = start.toCharArray().toList
        
        val rules = lines.drop(2).map(line => (line(0), line(1)) -> line(6)).toMap

        println(s"Rules: ${rules}")

        val max_generation = 20
        val generations = (1 to max_generation)

        var buffer = polymer.to(ListBuffer)

        var counter = new CharCounter()
        var safeChar = polymer(0);
        counter.count(safeChar)
      
        for (c <- polymer.drop(1)) {
            
            var levels = new Stack[(Char, Int)]()
            levels.prepend((c, 0))
            println(s"New round ${Calendar.getInstance().getTime()}")

            while (levels.size > 0) {
                var level = levels.head._2
                                
                while (level < max_generation) {
                    val headChar = levels.head._1
                    val matchingRuleOption = rules.get((safeChar, headChar))

                    level = level + 1
                    levels.update(0, (headChar, level))

                    matchingRuleOption.foreach(newChar => levels.prepend((newChar, level)))
                    /*
                    levels = matchingRuleOption match {                           
                        case Some(newChar) => { 
                            levels.prepend((newChar, level))
                            levels
                        }
                        case None => { 
                            levels
                        }
                    }
                    */
                }
                
                // level == 40
                safeChar = levels.pop()._1
                counter.count(safeChar)                           
            }
        }

        counter.print();
    }

    class CharCounter {
        var counter: HashMap[Char, Long] = new HashMap()
        var num: Long = 0

        def count(c: Char): Unit = {
            if (this.counter.contains(c)) {
                val oldValue = this.counter(c);
                this.counter(c) = oldValue + 1
            } else {
                this.counter += (c -> 1)
            }

            num = num + 1
            if (num % 10000000 == 0)
                println(s"<${num} counted>");
        }

        def print(): Unit = {
            val minItem = counter.minBy(_._2);
            val maxItem = counter.maxBy(_._2);

            println(s"Counter: ${counter}, min ${minItem}, max ${maxItem}, res = ${maxItem._2 - minItem._2}")
        }
    }
}

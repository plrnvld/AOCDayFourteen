import scala.io.Source
import scala.collection.mutable.ListBuffer

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val start = lines(0)

        var polymer = start.toCharArray().toList
        
        val rules = lines.drop(2).map(line => Rule(line(0), line(1), line(6)))

        val generations = (1 to 10)

        var buffer = polymer.to(ListBuffer)

        for (gen <- generations) {
            buffer = expand(buffer, rules)
            println(s"After step ${gen} : ${buffer}")
        }
        
        val elementCount = buffer.groupBy(identity).mapValues(_.size)

        val maxElem = elementCount.maxBy(_._2)
        val minElem = elementCount.minBy(_._2)

        println(s"Finished: max ${maxElem}, min ${minElem} => ${maxElem._2 - minElem._2}")
    }

    def expand(polymer: ListBuffer[Char], rules: List[Rule]): ListBuffer[Char] = {
        var collected = new ListBuffer[Char]()

        val indices = (0 to polymer.size - 2)
        for (index <- indices) {
            val charOne = polymer(index)
            val charTwo = polymer(index + 1)
            val matchingRuleOption = rules find (r => r.charOne == charOne && r.charTwo == charTwo)

            collected += charOne
            matchingRuleOption.foreach(rule => collected += rule.charToInsert)
        }

        collected += polymer.last
        collected
    }

    case class Rule(charOne: Char, charTwo: Char, charToInsert: Char) {
    }
}

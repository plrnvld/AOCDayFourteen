import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import java.util.Calendar
import scala.runtime.LongRef

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val start = lines(0)

        val polymer = start.toCharArray().toList
        val rules = lines.drop(2).map(line => (line(0), line(1)) -> line(6)).toMap
        
        val maxGeneration = 40
        val levelsToCache = 22

        val cache = buildCache(rules, levelsToCache)
        
        countPolymer(polymer, maxGeneration, cache, levelsToCache, rules).print();
    }

    def buildCache(rules: Map[(Char, Char), Char], levelsToCache: Int): CountCache = {
        var allCharSet: Set[Char] = Set();
        
        rules.keys.foreach(key => { 
            allCharSet.add(key._1)
            allCharSet.add(key._2) 
        })
        
        for (v <- rules.values) {
            allCharSet.add(v)
        }      

        val cache = new CountCache()

        println(s"Caching ${allCharSet.size} x ${allCharSet.size} for levels to cache ${levelsToCache}")
        
        for (charOne <- allCharSet) {
            for (charTwo <- allCharSet) {
                val chars = List(charOne, charTwo)
                val count = countPolymer(chars, levelsToCache, cache, -1, rules) // -1 = don't use caching
                count.uncount(charOne)
                count.uncount(charTwo) // only count expanded chars
                cache.add(charOne, charTwo, levelsToCache, count)
                print(".")
            }
        }        
        println()
        println("Finished caching")

        cache
    }

    def countPolymer(polymer: List[Char], maxGeneration: Int, cache: CountCache, levelsToCache: Int, rules: Map[(Char, Char), Char]): CharCounter = {
        var counter = new CharCounter()
        var safeChar = polymer(0);
        counter.count(safeChar)
      
        for (c <- polymer.drop(1)) {
            
            var levels = new Stack[(Char, Int)]()
            levels.prepend((c, 0))

            if (levelsToCache > 0)
                println(s"New round ${Calendar.getInstance().getTime()}")

            while (levels.size > 0) {
                var level = levels.head._2

                var cacheEncountered = false 
                                
                while (level < maxGeneration && !cacheEncountered) {
                    val headChar = levels.head._1

                    level = level + 1
                    levels.update(0, (headChar, level))
                    val levelToCheckCache = maxGeneration - levelsToCache + 1 
                    if (levelsToCache != -1 && level == maxGeneration - levelsToCache + 1) { 
                        val count = cache.getCountCache(safeChar, headChar, levelsToCache).get
                        counter.addCounter(count)
                        cacheEncountered = true
                    } else {
                        val matchResult = rules.get((safeChar, headChar))
                        matchResult.foreach(newChar => levels.prepend((newChar, level)))
                    }
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
            this.counter.get(c) match {
                case Some(oldValue) => this.counter(c) = update(oldValue)
                case None => this.counter += (c -> update(0))
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

    class CountCache {
        var cache: HashMap[(Char, Char, Int), CharCounter] = new HashMap()

        def getCountCache(charOne: Char, charTwo: Char, remaining: Int): Option[CharCounter] = {
            cache.get((charOne, charTwo, remaining))
        }

        def add(charOne: Char, charTwo: Char, remaining: Int, counter: CharCounter): Unit = {
            this.cache += ((charOne, charTwo, remaining) -> counter)
        }
    }
}

package parser

import scala.util.parsing.combinator._
import scala.collection.immutable.StringOps
import scala.collection.mutable.HashMap
import scala.collection.immutable.List
import java.io.BufferedReader
import java.io.FileReader
import java.math


abstract class  Symbol
case class NilSymbol()               extends Symbol
case class NumberSymbol( number: String ) extends Symbol
case class OtherSymbol( char: String )  extends Symbol
case class WordSymbol( word: String )   extends Symbol
case class ListSymbol( symlist: List[Symbol] ) extends Symbol
case class ParEndSymbol( ) extends Symbol

object noSymbol extends NilSymbol

class SymbolParser extends RegexParsers {
  def lineend: Parser[NilSymbol]    = """\r?\n""".r ^^ { s => NilSymbol()}
  def number:  Parser[Symbol]       = """\d+""".r  ^^ { n => NumberSymbol( n ) }
  def other:   Parser[OtherSymbol]  = """.""".r ^^ { c => OtherSymbol( c ) }
  def paraend: Parser[ParEndSymbol] = lineend~lineend ^^ { l1 => ParEndSymbol() }
  def symbols: Parser[ListSymbol]   = rep(symbol) ^^ { l => ListSymbol(l ) }
  def symbol:  Parser[Symbol]       = word | number | space | paraend | lineend | other
  def space:   Parser[NilSymbol]    = whiteSpace ^^ { s => NilSymbol()}
  def word:    Parser[WordSymbol]   = """[A-Za-z][A-Za-z']*""" .r ^^ { w => WordSymbol( w.toLowerCase() ) }
}

class SymbolFrequencies {
  var total:   Int = 0
  var symbols: HashMap[Symbol,Int] = new HashMap[Symbol,Int]()

  def addSymbol( symbol: Symbol ) = {
    val symbolFreq = symbols.get(symbol)

    symbolFreq match {
      case Some(freq) => { total = total + 1
			   symbols.update(symbol, freq+1)
//			  println( "addSymbol1: ["+symbol+ "->"+ freq+1)
			}
      case None       => { total = total + 1
                           symbols.update(symbol, 1)
//			  println( "addSymbol2: ["+symbol+ "->"+ 1)
			}
    }
  }

  def selectSymbol(): Symbol = {
    def getSymbol( iter: Iterator[(Symbol,Int)], selectVal: Int ): Symbol = {
      if ( iter.hasNext ) {
	iter.next() match {
	  case (key, value) => if ( selectVal <= value ) {
//	    println( "getSymbol: symbol = " + key )
	    key
	  } else {
	    getSymbol( iter, selectVal - value )
	  }
	}
      } else {
	noSymbol
      }
    }

    val randVal    = Math.round( total * Math.random ).toInt
    val symbolIterator = symbols.iterator
//    println( "selectSymbol: total = "+total+"  randVal = "+randVal)
    getSymbol( symbolIterator, randVal )
  }
    
  override def toString: String = {
    "SymbolFrequencies("+total+",HashMap(" + 
    symbols.foldRight(""){ case (kv, a) => a+","+kv._1+"->"+kv._2}
    "))"
  }
}

object SymbolParser extends SymbolParser {
  val parser = new SymbolParser

  def main( args: Array[String] ) {
    val wordsInKey = 3;
    if ( args.length < wordsInKey ) {
      println( "usage:  parser.SymbolParser corpus word1 word2..." )
    } else {
      val reader= new BufferedReader(new FileReader( args(0)))

      val parse_result = parse( symbols, reader ) match {
	case Success( ListSymbol(symlist), _ ) => symlist.sliding(wordsInKey) .toList
	case Error( msg, _ ) => { println( "Error: "+msg ); return }
      }

      val frequencyMap = new HashMap[List[Symbol],SymbolFrequencies]()

      parse_result map { symlist => addSymbolToFreqMap( symlist, frequencyMap ) }

//      frequencyMap foreach { item => println( item )}

      val wordList = for ( i <- 1 to wordsInKey-1 ) yield WordSymbol(args(i))
      //List( WordSymbol(args(1)), WordSymbol(args(2)))
//      print( args :\ (""){_+_} )
      genSymbols( wordList.toList, 200, 70 - args(1).length - args(2).length - 2, frequencyMap )
      println()
    }
  }

  def genSymbols( wordList: List[Symbol],
		 number: Int,
		 lineSize: Int,
		 frequencyMap: HashMap[List[Symbol],SymbolFrequencies]) {
    if ( number > 0 ) {
      val nextSymbol: Symbol = selectNextSymbol( wordList, frequencyMap )
      val nextList : List[Symbol] = wordList.tail ::: List(nextSymbol)

      nextSymbol match {
	case NumberSymbol( n ) => {
	  val outstr=n.toString;
	  if ( lineSize - outstr.length - 1 < 0 ) {
	    println
	    print( " "+outstr)
	    genSymbols( nextList, number -1, 70-outstr.length-1,       frequencyMap)
	  } else {
	    print( " "+outstr)
	    genSymbols( nextList, number -1, lineSize-outstr.length-1, frequencyMap)
	  }}
	case OtherSymbol( c ) =>
	  if ( lineSize < 1 ) {
	    println
	    print(c)
	    genSymbols( nextList, number -1, 69,		       frequencyMap )
	  } else {
	    print(c)
	    genSymbols( nextList, number -1, lineSize - 1,             frequencyMap )
	  }
	case WordSymbol( outstr ) => {
	  if ( lineSize - outstr.length - 1 < 0 ) {
	    println
	    print( " "+outstr)
	    genSymbols( nextList, number -1, 70-outstr.length-1,      frequencyMap)
	  } else {
	    print( " "+outstr)
	    genSymbols( nextList, number -1, lineSize-outstr.length-1,frequencyMap)
	  }}
      }
    } else {
      return
    }
  }
    
  def addSymbolToFreqMap( symbolList: List[Symbol],
			 hm: HashMap[List[Symbol],SymbolFrequencies] ) = {
      val key = symbolList.dropRight(1);
      val symbol = symbolList.last;
      hm.get(key) match {
        case Some(symFreqs) => symFreqs.addSymbol(symbol)
        case None           => { val frequencyMap = new SymbolFrequencies()
                                  frequencyMap.addSymbol( symbol )
		                              hm.update( key, frequencyMap )
		                            }
     }
  }
  
  def lookup(symbolList: List[Symbol],
	     hm: HashMap[List[Symbol],SymbolFrequencies] ) = {
      val key = symbolList
    val rtn = hm.get( key )
//    println( "lookup: key=("+key+")->"+rtn )
    rtn
  }

  def selectNextSymbol( symbolList: List[Symbol],
		       hm: HashMap[List[Symbol],SymbolFrequencies] ): Symbol = {
    lookup( symbolList, hm ) match {
      case Some( symFreqs ) => symFreqs.selectSymbol()
      case None             => noSymbol
    }
  }
}
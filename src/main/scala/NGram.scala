import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import scala.collection.immutable.List
import java.io.BufferedReader
import java.io.FileReader
import scala.math

case class Arguments( infiles: List[String], ngramWords: Int, genWords: Int, startWords: List[String])

object NGram  {
  val parser = new SymbolParser
  
  def main( args: Array[String] ) {
    def parseArgs( results: Arguments, args: List[String] ): Arguments =
      if ( args.isEmpty ) results
      else
	args match {
	  case "-d"::fileName::rest => parseArgs( Arguments(results.infiles:::List(fileName),
							    results.ngramWords,
							    results.genWords,
							    results.startWords), rest )
	  case "-n"::nwords::rest  => parseArgs( Arguments(results.infiles,
							   nwords.toInt,
							   results.genWords,
							   results.startWords), rest )
	  case "-w"::twords::rest  => parseArgs( Arguments(results.infiles,
							   results.ngramWords,
							   twords.toInt,
							   results.startWords), rest )
	  case word::rest          => parseArgs( Arguments(results.infiles,
							   results.ngramWords,
							   results.genWords,
							   results.startWords:::List(word)), rest )
	  case Nil                 => results
	}
    
    val ngramWords = 2
    val numWords   = 200
    val pargs = parseArgs(Arguments(List():List[String],
				    ngramWords,
				    numWords,
				    List():List[String]), args.toList)
//    println( "args:  infiles:    "+pargs.infiles)
//    println( "       ngramWords: "+pargs.ngramWords)
//    println( "       genWords:   "+pargs.genWords)
//    println( "       startWords: "+pargs.startWords)
			  
    if ( pargs.infiles.length == 0 || pargs.startWords.length != pargs.ngramWords - 1 ) {
      println( "usage:  NGram {-d corpus}+ {-n numOutputWords}? {-t wordsInNGram}? word1 word2..." )
      println( "        Use multiple -d options to include multiple input corpii")
      println( "        The number of words used to start generation must be one less than" )
      println( "          the number of words in the NGram." )
    } else {
      val dictionary = new Dictionary()
      val y = for {inFile <- pargs.infiles
		   reader= new BufferedReader(new FileReader( inFile))
		   parse_result = parser.parse( parser.symbols, reader ) match {
		     case parser.Success( ListSymbol(symlist), _ ) =>
		       symlist.sliding(pargs.ngramWords).toList
		     case parser.Error( msg, _ )                   =>
		       { println( "Error: "+msg ); return }
		   }
		   x = parse_result map { symlist => dictionary.addSymbol( symlist ) }
		 } yield( x )
      val wordList = for ( w <- pargs.startWords ) yield WordSymbol(w)
      val startphrase = (wordList.head.toString /: wordList.tail)(_+" "+_.toString)
      print( startphrase )
      dictionary. genSymbols( wordList.toList,
			     pargs.genWords - pargs.ngramWords - 1,
			     70 - startphrase.length )
      println()
    }
  }
}

abstract class  Symbol
case class NilSymbol()                         extends Symbol {
  override def toString: String = ""
}
case class NumberSymbol( number: String )      extends Symbol {
  override def toString: String = number.toString
}
case class OtherSymbol( char: String )         extends Symbol {
  override def toString: String = char.toString
}
case class WordSymbol( word: String )          extends Symbol {
  override def toString: String = word
}
case class ListSymbol( symlist: List[Symbol] ) extends Symbol {
  override def toString: String = (symlist.head.toString /: symlist.tail)(_+" "+_.toString)
}
case class ParEndSymbol( )                     extends Symbol {
  override def toString = " "
}

object noSymbol extends NilSymbol

class SymbolParser extends RegexParsers {
  def lineend: Parser[NilSymbol]    = """\r?\n""".r               ^^ { s => NilSymbol()}
  def number:  Parser[Symbol]       = """\d+""".r                 ^^ { n => NumberSymbol( n ) }
  def other:   Parser[OtherSymbol]  = """.""".r                   ^^ { c => OtherSymbol( c ) }
  def paraend: Parser[ParEndSymbol] = lineend~lineend             ^^ { l1 => ParEndSymbol() }
  def space:   Parser[NilSymbol]    = whiteSpace                  ^^ { s => NilSymbol()}
  def symbol:  Parser[Symbol]       = word | number | space | paraend | lineend | other
  def symbols: Parser[ListSymbol]   = rep(symbol)                 ^^ { l => ListSymbol(l) }
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
			 }
      case None       => { total = total + 1
                           symbols.update(symbol, 1)
			 }
    }
  }

  def selectSymbol(): Symbol = {
    def getSymbol( iter: Iterator[(Symbol,Int)], selectVal: Int ): Symbol = {
      if ( iter.hasNext ) {
	iter.next() match {
	  case (key, value) => if ( selectVal <= value ) {
	    key
	  } else {
	    getSymbol( iter, selectVal - value )
	  }
	}
      } else noSymbol
    }

    val randVal    = scala.math.round( total * scala.math.random ).toInt
    val symbolIterator = symbols.iterator
    getSymbol( symbolIterator, randVal )
  }
    
  override def toString: String = {
    "SymbolFrequencies("+total+",HashMap(" + 
    symbols.foldRight(""){ case (kv, a) => a+","+kv._1+"->"+kv._2}
    "))"
  }
}

class Dictionary extends HashMap[List[Symbol],SymbolFrequencies] {

  def addSymbol( symbolList: List[Symbol] ) = {
    val key = symbolList.dropRight(1);
    val symbol = symbolList.last;
    get(key) match {
      case Some(symFreqs) => symFreqs.addSymbol(symbol)
      case None           => { val frequencyMap = new SymbolFrequencies()
                              frequencyMap.addSymbol( symbol )
                              update( key, frequencyMap )
		            }
    }
  }

  def genSymbols( wordList: List[Symbol],
		 number: Int,
		 lineSize: Int ) {
    if ( number > 0 ) {
      val nextSymbol: Symbol = selectNextSymbol( wordList )
      val nextList : List[Symbol] = wordList.tail ::: List(nextSymbol)
      val outstr = nextSymbol.toString
      if ( lineSize - outstr.length -1 < 0 ) {
	println;
	print( outstr )
	genSymbols( nextList, number -1, 70-outstr.length )
      } else {
	print( " "+outstr )
	genSymbols( nextList, number -1, lineSize-outstr.length-1)
      }
    } else return
  }

  def lookup(symbolList: List[Symbol] ) = {
    get( symbolList )
  }

  def selectNextSymbol( symbolList: List[Symbol] ): Symbol = {
    lookup( symbolList ) match {
      case Some( symFreqs ) => symFreqs.selectSymbol()
      case None             => noSymbol
    }
  }
}


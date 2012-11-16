NGram is an n-gram generator written in Scala.  It requires one or more
input files which it uses to generate a dictionary of n-grams.  You then
provide n-1 words to start the text generation.

Specifically the command line arguments are:

    -d input_text_file (may be repeated as desired)
    -n ngram_size       Number of symbols in n-gram (default: 3)
    -w gen_words        Number of symbols to generate (default: 200)
    word0 word1 ...     ngram_size-1 symbols to start things off

NGram contains a simple parser that recognizes words and other symbols
in the one or more files specified by the -d argument(s).

It uses a pseudo random numbur generator to select the next symbol
from a list of symbols associated with the previous ngram.

Project Gutenberg, http://gutenberg.org, has many texts that you can
download and use as a source of text to use for dictionaries.

This directory contains an SBT build file if you want to build it
using SBT (www.scala-sbt.org).


Examples

scala -howtorun:object NGram -d data/gettysburg.txt fourscore and

Will generate two hundred symbols of trigrams from Lincoln's
Gettysburg address.


scala -howtorun:object NGram -d data/gettysburg.txt -n 5 fourscore and seven years

Will do a pretty good job of reproducing the entire speech.


scala -howtorun:object NGram -d data/TomSawyer.txt -d data/ChristmasCarol.txt perfect stillness

Tom Sawyer meets Tiny Tim!


Have fun and enjoy.

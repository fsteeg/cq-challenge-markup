#!/bin/sh
exec scala -classpath target/scala_2.8.0/cq-challenge-markup_2.8.0-0.1.0.jar $0 $@
!#

import com.quui.markup.{MarkupParser,MarkupBackend}
import scala.io.Source
import scala.xml.PrettyPrinter

if(args.size != 1) println("Pass a single argument: the name of the markup file to process")
else {
  /* Some configurable items: */
  val processor = new MarkupParser/*(sub = """note|footnote""".r)*/
  val pretty = new PrettyPrinter(100, 2)
  val input = Source.fromFile(args(0)/*, UTF-8*/).mkString
  /* Main driver program - parse and output as XML: */
  val result = processor.parseMarkup(input)
  val xml = pretty format MarkupBackend.toXml(result)
  println(xml)
}

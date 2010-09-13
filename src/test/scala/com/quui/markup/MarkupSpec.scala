package com.quui.markup
import scala.xml.PrettyPrinter
import scala.util.parsing.combinator.Parsers
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Markup._
import java.io.File
import scala.io.Source._
import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class MarkupSpec extends MarkupParser with Spec with ShouldMatchers {
  
  describe("The markup processor") {
    it("can parse and export markup input"){
      val parsed = Markup.parse(fromFile("terms.txt").mkString, sub = "note|footnote".r) // 'sub' is optional
      val output = Markup.toXml(parsed, pretty = false) // 'pretty' is optional
    }
    it("can convert a markup file passed as a command-line argument to an XML representation"){
      Markup.main(Array("terms.xml"))
    }
  }

  val input = """* the first header

a first paragraph.

  some famous words

** the second header

and another"""

  val pretty = new PrettyPrinter(200, 2)

  describe("The Markup language") {
    it("uses CR (U+000D), CR/LF, (U+000D U+000A), or LF (U+000A) for line termination") {
      expect(classOf[Success[_]]) { parseAll(newLine, "\u000D").getClass }
      expect(classOf[Success[_]]) { parseAll(newLine, "\u000D\u000A").getClass }
      expect(classOf[Success[_]]) { parseAll(newLine, "\u000A").getClass }
    }
  }

  describe("The Markup parser") {
    it("can parse markup input into an internal tree representation") {
      expect(
        Body(List(
          H(1, List(TextElement("the first header"))),
          P(List(TextElement("a first paragraph."))),
          BlockQuote(List(
            P(List(TextElement("some famous words"))))),
          H(2, List(TextElement("the second header"))),
          P(List(TextElement("and another")))))
        ) {
        parseMarkup(input)
      }
    }
    it("parses 'note' tags as sub-documents in its default configuration") {
      expect(<p><note><p>Text</p></note></p>) { MarkupBackend.toXml(parseAll(p, """\note{Text}""").get) }
      expect(<p><foot>Text</foot></p>) { MarkupBackend.toXml(parseAll(p, """\foot{Text}""").get) }
    }
    it("can parse custom tags as sub-documents if specified in a pattern") {
      object CustomParser extends MarkupParser(sub = "note|foot".r) {
        expect(<p><note><p>Text</p></note></p>) { MarkupBackend.toXml(parseAll(p, """\note{Text}""").get) }
        expect(<p><foot><p>Text</p></foot></p>) { MarkupBackend.toXml(parseAll(p, """\foot{Text}""").get) }
      }
    }
    it("supports simple links") {
      expect(<link>text</link>) {
        val parse = parseAll(linkSimple, """[text]""")
        MarkupBackend.toXml(checked(parse))
      }
    }
    it("supports links with keys") {
      expect(<link>text<key>key</key></link>) {
        val parse = parseAll(linkWithKey, """[text|key]""")
        MarkupBackend.toXml(checked(parse))
      }
    }
    it("supports link defs") {
      expect(<link_def><link>text</link><url>http://www.example.com/text/</url></link_def>) {
        val parse = parseAll(linkDef, """[text] <http://www.example.com/text/>""")
        MarkupBackend.toXml(checked(parse))
      }
    }
    it("can parse the text files included in the project") {
      for (
        file <- new File(".").listFiles;
        if file.getName.endsWith(".txt") || file.getName.equals("README")
      ) { expect(classOf[Body]) { parseMarkup(fromFile(file).mkString).getClass } }
    }
  }

  describe("The Markup model") {
    it("can be exported to an XML representation") {
      val xml = <body>
                  <h1>the first header</h1>
                  <p>a first paragraph.</p>
                  <blockquote>
                    <p>some famous words</p>
                  </blockquote>
                  <h2>the second header</h2>
                  <p>and another</p>
                </body>
      expect(pretty format xml) {
        val res = parseAll(body, input).get
        println(pretty format MarkupBackend.toXml(res))
        pretty format MarkupBackend.toXml(res)
      }
    }
    it("corresponds to the samples given in files") {
      val folder = new File("tests")
      for (
        file <- folder.listFiles;
        if file.getName.endsWith(".txt");
        txt = file;
        xml = new File(txt.getAbsolutePath.replace(".txt", ".xml"))
      ) {
        val input = fromFile(txt).mkString
        println("Input:\n" + input)
        val parsed = parseMarkup(input)
        println("Parsed: " + parsed)
        val parsedXmlSpec = pretty format MarkupBackend.toXml(parsed)
        val parsedXmlSample = pretty format MarkupBackend.toXmlSample(parsed)
        println("Output: " + parsedXmlSpec)
        val correct = XML.loadFile(xml)
        println("Correct: " + correct)
        print("[Testing] %s ".format(txt.getName))
        expect(pretty format correct) { parsedXmlSpec }
        expect(pretty format correct) { parsedXmlSample }
        println("[OK]")
      }
    }
  }

}
import scala.xml.PrettyPrinter
import scala.util.parsing.combinator.Parsers
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import MarkupModel._
import MarkupBackend._
import java.io.File
import scala.io.Source
import scala.xml.XML

@RunWith(classOf[JUnitRunner])
class MarkupParserSpec extends MarkupParser with Spec with ShouldMatchers {

  val input = """* the first header

a first paragraph.

  some famous words

** the second header

and another

"""

  val pretty = new PrettyPrinter(200, 2)

  describe("The Markup language") {
    it("uses CR (U+000D), CR/LF, (U+000D U+000A), or LF (U+000A) for line termination") {
      expect(classOf[Success[Any]]) { parseAll(newLine, "\u000D").getClass }
      expect(classOf[Success[Any]]) { parseAll(newLine, "\u000D\u000A").getClass }
      expect(classOf[Success[Any]]) { parseAll(newLine, "\u000A").getClass }
    }
  }

  describe("The Markup parser") {
    it("can parse markup input into an internal tree representation") {
      expect(
        Body(List(
          H(1, List(TextMarkup("the first header"))),
          P(List(TextMarkup("a first paragraph."))),
          BlockQuote(List(
            P(List(TextMarkup("some famous words"))))),
          H(2, List(TextMarkup("the second header"))),
          P(List(TextMarkup("and another")))))
        ) {
        val res = parseAll(body, input)
        res.getOrElse(res)
      }
    }
    it("parses 'note' tags as sub-documents in its default configuration") {
      expect(<p><note><p>Text</p></note></p>) { toXml(parseAll(p, """\note{Text}""").get) }
      expect(<p><foot>Text</foot></p>) { toXml(parseAll(p, """\foot{Text}""").get) }
    }
    it("can parse custom tags as sub-documents if specified in a pattern"){
      object CustomParser extends MarkupParser(sub = "note|foot".r) {
        expect(<p><note><p>Text</p></note></p>) { toXml(parseAll(p, """\note{Text}""").get) }
        expect(<p><foot><p>Text</p></foot></p>) { toXml(parseAll(p, """\foot{Text}""").get) }
      }
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
        println(pretty format toXml(res))
        pretty format toXml(res)
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
        val input = Source.fromFile(txt).mkString
        println("Input:\n" + input)
        val parsed = parseAll(body, input)
        println("Parsed: " + parsed)
        val parsedXmlSpec = pretty format toXml(parsed.get)
        val parsedXmlSample = pretty format toXmlSample(parsed.get)
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
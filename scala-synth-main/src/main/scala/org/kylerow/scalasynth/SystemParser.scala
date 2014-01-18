package org.kylerow.scalasynth

import scala.util.parsing.combinator.JavaTokenParsers
import org.kylerow.scalasynth.midi.Midi
import org.kylerow.scalasynth.module._
import org.kylerow.scalasynth.audio._
import org.kylerow.scalasynth.note._

object SystemParser extends JavaTokenParsers {

  def system: Parser[Midi] = input ~ module ~ output ^^ { case i ~ mod ~ o => i >> mod; i
  //o.attachSender(mod); i 
  }
  
  def input: Parser[Midi] = "midi" ^^ { case _ => Midi() }
    
  def module: Parser[BasicModule] = "BasicOscillator(" ~> wave <~ ")" ^^ { case wave => val bm=BasicOscillator("basic_oscillator1")
		  bm.setWave(bm.sine); bm }

  def output: Parser[Audio] = "audio" ^^ { case _ => Audio() }
    
  def wave: Parser[String] = "sine"

    def note: Parser[Note] = predefined_note
  
  def predefined_note: Parser[Note] = ("a4" | "c5" | "d5") ^^ { case "a4" => a4; case "c5" => c5; case "d5" => d5 }
  
  def parse(text: String) {
    
    SystemParser.parseAll(system, text) match {
      
      case Success(midi,_) => midi.playNote(d5)
      case Failure(msg,_) => println(msg)
      case Error(msg,_) => println(msg)
    }
  } 
}
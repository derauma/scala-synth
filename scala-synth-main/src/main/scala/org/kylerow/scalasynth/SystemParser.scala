package org.kylerow.scalasynth

import scala.util.parsing.combinator.JavaTokenParsers
import org.kylerow.scalasynth.midi.Midi
import org.kylerow.scalasynth.module._
import org.kylerow.scalasynth.audio._

object SystemParser extends JavaTokenParsers {

  def system: Parser[Midi] = input ~ module ~ output ^^ { case i ~ mod ~ o => i >> mod; o.attachSender(mod); i }
  
  def input: Parser[Midi] = "midi" ^^ { case _ => Midi() }
    
  def module: Parser[BasicModule] = basic_oscillator
  
  def basic_oscillator: Parser[BasicModule] = opt(name) ~ shape ^^ { case name ~ shape => 

    	val bm=BasicOscillator(name.getOrElse("module1"))
		  val shape1 = shape match {
		  	case "square" => bm.setWave(bm.square)    
		  	case "sine" => bm.setWave(bm.sine)    
		  	case "saw" => bm.setWave(bm.saw)
  			}
  		bm
  }

  def output: Parser[Audio] = "audio" ^^ { case _ => Audio() }
    
  def shape: Parser[String] = "square" | "sine" | "saw"
  
  def name: Parser[String] = ident <~ ":"
  
  def apply(text: String): Midi = {
    
    parseAll(system, text) match {
      
      case Success(midi,_) => midi
      case Failure(msg,_) => throw new Exception(msg)
      case Error(msg,_) => throw new Exception(msg)
    }
  }

}

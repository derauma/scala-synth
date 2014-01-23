package org.kylerow.scalasynth

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class SystemParserSpec 
	extends FlatSpec 
	with ShouldMatchers
	{

	"Parse" should "parse a system instance and return the midi" in {

		// act
		
		val midi=SystemParser("midi square1:square audio")
	}

	"Parse" should "not parse a system instance and throw an exception" in {

		// act
		
	  // should throw exception.  how do I test for that?
		val midi=SystemParser("midi invalid_module_name audio")
	}
}
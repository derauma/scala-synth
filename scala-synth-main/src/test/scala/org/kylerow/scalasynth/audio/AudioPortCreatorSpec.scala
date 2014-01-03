package org.kylerow.scalasynth.audio

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalamock.scalatest.MockFactory

@RunWith(classOf[JUnitRunner])
class AudioPortCreatorSpec 
extends FlatSpec 
	with ShouldMatchers
	with MockFactory{

  "create" should "get function from map and execute" in{
    // arrange
    val mockAudioPort = mock[AudioPort]
    val mockCreateFunction = mockFunction[AudioPortOptions,AudioPort];
    val map = Map("test1" -> mockCreateFunction)
    (mockCreateFunction) expects(*) returns(mockAudioPort);
    
    val audioPortCreator = new AudioPortCreator();
    audioPortCreator.creatorFunctionMap = map;
    
    // act
    val result = audioPortCreator.create("test1",new AudioPortOptions)
    assert (result eq mockAudioPort)
  }
}
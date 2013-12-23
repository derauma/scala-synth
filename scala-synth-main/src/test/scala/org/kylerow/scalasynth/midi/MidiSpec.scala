package org.kylerow.scalasynth.midi

import org.scalamock.scalatest.MockFactory
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class MidiSpec 
	extends FlatSpec 
	with ShouldMatchers
	with MockFactory{
  
  "registerReceiver" should 
  "provide all transmitter providers and " +
  "the Midi object itself to the connector" in {
	// arrange
    val mockEventProviderLocator = mock[EventProviderLocator];
    val mockEventProvider = mock[EventProvider]
   
    val mockEventConnector = mock[EventConnector]
    val midi = new Midi()
    val providers = List(mockEventProvider)
    val resultProviders = providers ::: List(midi);
    val mockEventReceiver = mock[EventReceiver]
    
    (mockEventConnector.connect _) expects(resultProviders,mockEventReceiver)
    (mockEventProviderLocator.getAllSystemMidiTransmitters _) expects() returns(providers)
    midi.eventProviderLocator = mockEventProviderLocator
    midi.eventConnector = mockEventConnector
    
    // act
    midi >> mockEventReceiver
  }
}
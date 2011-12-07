package com.metatrope.tools.terraria

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TerrariaWorld10Test extends FlatSpec {
	val world = new TerrariaWorld("world10.wld")
	
	"The test World" should "be called 'Known World'" in {
	    assert(world.header.name.equals("Known World"))
	    assert(world.footer.name.equals("Known World"))
	}
	
	"The test World" should "is medium sized" in {
	    assert(world.header.sizex == 6300)
	    assert(world.header.sizey == 1800)
	}
}

@RunWith(classOf[JUnitRunner])
class TerrariaWorld11Test extends FlatSpec {
    val world = new TerrariaWorld("world11.wld")
    
    "The test World" should "be called 'Known World'" in {
        assert(world.header.name.equals("Known World"))
        assert(world.footer.name.equals("Known World"))
    }
    
    "The test World" should "is medium sized" in {
        assert(world.header.sizex == 6300)
        assert(world.header.sizey == 1800)
    }
}


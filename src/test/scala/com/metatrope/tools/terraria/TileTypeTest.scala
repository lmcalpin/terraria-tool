package com.metatrope.tools.terraria

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TileTypeTest extends FlatSpec with ShouldMatchers {
	"A TileType" should "locatable by an integer code" in {
	    TileType.withId(1, TileType.Unknown) should equal (TileType.Stone)
	}
	
	it should "return a defult value when the lookup code is invalid" in {
	    TileType.withId(10000, TileType.Unknown) should equal (TileType.Unknown)
	}
}


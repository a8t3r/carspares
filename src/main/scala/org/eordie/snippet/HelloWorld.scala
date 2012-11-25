package org.eordie
package snippet

import net.liftweb._
import http.js.JE.{JsRaw, JsArray, JsObj}
import http.js.{JsObj, JsCmd}
import http.js.JsCmds.{OnLoad, Script, JsCrVar, RedirectTo}
import xml.NodeSeq

class HelloWorld {
  // replace the contents of the element with what map to render
  def howdy = renderGoogleMap()

  // converts a the location into a JSON Object
  def makeLocation(title: String, lat: String, lng: String): JsObj = {
    JsObj(("title", title),
      ("lat", lat),
      ("lng", lng))
  }

  // called by renderGoogleMap which passes the list of locations
  // into the javascript function as json objects
  def ajaxFunc(locobj: List[JsObj]): JsCmd = {
    JsCrVar("locations", JsObj(("loc", JsArray(locobj: _*)))) & JsRaw("drawmap(locations)").cmd
  }

  // render the google map
  def renderGoogleMap(): NodeSeq = {
    // setup some locations to display on the map
    val locations: List[JsObj] = List(makeLocation("loc1", "43.508721", "39.868672"))

    // where the magic happens
    (<head>
      {Script(OnLoad(ajaxFunc(locations)))}
    </head>)
  }
}

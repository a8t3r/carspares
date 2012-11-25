package org.eordie
package snippet

import scala.xml.NodeSeq

import net.liftweb.util.Props

object ProductionOnly {
  def render(in: NodeSeq): NodeSeq =
    if (Props.productionMode) in
    else NodeSeq.Empty
}

object ThrowException {
  def render =
    throw new Exception("This is only a test.")
}
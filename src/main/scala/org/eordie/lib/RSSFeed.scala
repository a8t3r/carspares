package org.eordie.lib

import net.liftweb.http._
import org.eordie.model.car.Offer
import net.liftweb.http.rest.XMLApiHelper
import xml.NodeSeq

/**
 *
 * @author Alexandr Kolosov
 * @since 11/29/12
 */
object RSSFeed extends XMLApiHelper {

  def dispatch: LiftRules.DispatchPF = {
    case Req("rss" :: Nil, "", GetRequest) => () => showOffers()
    case Req("rss" :: _ :: Nil, "",  _) => (() => BadResponse())
  }

  def showOffers(): LiftResponse = {
    val elems: NodeSeq = (for (offer <- Offer.findAll) yield
      <item>
        <title>{offer.title.is}</title>
        <description>{offer.description.is}</description>
        <link>{offer.viewHref}</link>
      </item>)

    AtomResponse(createTag(elems))
  }

  def createTag(in: NodeSeq) = {
    <rss version="2.0">
      <channel>
        <title>Carspares</title>
        <link>http://carspares.sochi.su</link>
        <description>Получение последних объявлений с сайта carspares.sochi.su</description>
        <language>ru</language>
        <generator>Lift WebFramework</generator>
        {in}
        </channel>
      </rss>
  }
}

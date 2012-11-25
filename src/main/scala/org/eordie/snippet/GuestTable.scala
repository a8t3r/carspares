package org.eordie.snippet

import org.eordie.model.car.{OfferType, Offer}
import xml.NodeSeq
import org.eordie.lib.DateHelper
import net.liftweb.http.S

/**
 *
 * @author Alexandr Kolosov
 * @since 11/21/12
 */
class GuestTable {

  def guest = {

    def firstImage(offer: Offer): NodeSeq = {
      val images = offer.images.is

      if (images.isEmpty) NodeSeq.Empty
      else <a href={S.hostAndPath + "/offers/view/" + offer.id}>
            <img src={"/serving/" + images.head + "_small"} height='100' width='100' />
           </a>
    }

    val nodes: NodeSeq = OfferType.managerOffers.flatMap((o: Offer) =>
      <tr>
        <td>
          <div style="height:100px;width:120px">
            {firstImage(o)}
          </div>
        </td>

        <td>
          <strong>{o.dealType.asHtml + ":   "}{o.title.asHtml}</strong><br/>
          <div style="width:700px">
            {
            val desc = o.description.is
            if (desc.length > 300) desc.take(300) + " ..."
            else desc
            }
          </div>
        </td>

        <td>
          <p>
            <small>{DateHelper.yesterdayOrToday(o.updatedAt.is)}</small><br/>
            <span class="label label-info">
              {o.estimatedPrice.viewString}
            </span>
          </p>
        </td>
      </tr>
    ).toSeq

    <table class="span12 table-striped"><tbody>{nodes}</tbody></table>
  }


}

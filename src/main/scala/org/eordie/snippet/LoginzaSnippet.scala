package org.eordie.snippet

import net.liftmodules.mongoauth.MongoAuth
import net.liftweb.http.S

/**
 *
 * @author Alexandr Kolosov
 * @since 11/12/12
 */
object LoginzaSnippet {

  val returnUrl: String = "http://loginza.ru/api/widget?overlay=loginza&token_url=" + S.hostAndPath + "/loginza"

  def render = <iframe src={returnUrl} style="width:359px;height:300px;" scrolling="no" frameborder="no"></iframe>

}

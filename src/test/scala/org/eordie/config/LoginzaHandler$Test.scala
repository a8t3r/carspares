package org.eordie.config

import net.liftweb.common.Box
import org.eordie.model.user.User
import org.eordie.BaseSpec
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonAST.JField
import net.liftweb.json.JsonAST.JObject
import org.eordie.lib.LoginzaHandler

/**
 *
 * @author Alexandr Kolosov
 * @since 11/12/12
 */
class LoginzaHandler$Test extends BaseSpec {

  val handler = LoginzaHandler

  val testData = JObject(List(
    JField("identity", JString("https://www.google.com/accounts/o8/id?id=AItOawmrMqKYg2HU2lLddsk_yd0YlttvBz_RUqQ")),
    JField("provider", JString("https://www.google.com/accounts/o8/ud")),
    JField("name", JObject(List(JField("first_name", JString("Александр")), JField("last_name", JString("Колосов")), JField("full_name", JString("Александр Колосов"))))),
    JField("language", JString("en")),
    JField("address", JObject(List(JField("home", JObject(List(JField("country", JString("RU")))))))),
    JField("email", JString("winter.schweigen@gmail.com")),
    JField("photo", JString("http://www.google.com/ig/c/photos/public/AIbEiAIAAABDCNmLtpaB5oz1IyILdmNhcmRfcGhvdG8qKDBkODJiNTY3OWY3ZTg5YTFmZDBhNTU2NzVkMDg0YWQ3YmYyNWM5NjcwAdO3xtB0_ud2z2J8Yzl6tLu7Ilks"))))

  "json parser" should {
    "recognize correct response" in {
      val result: Box[User] = handler.processJson(testData)
      assert(result.isDefined)

      val user: User = result.openTheBox
      assert("Александр Колосов" === user.username)
      assert("winter.schweigen@gmail.com" === user.email)
    }

    "recognize error response" in {
      val result = handler.processJson(JObject(List(JField("error_type", JString("token_validation")), JField("error_message", JString("Empty token value.")))))
      assert(result.isEmpty)
    }
  }
}

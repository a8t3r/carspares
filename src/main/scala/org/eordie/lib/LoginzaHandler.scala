package org.eordie.lib

import net.liftweb.http.{S, RedirectResponse}
import net.liftweb.common.{Empty, Box, Loggable}
import net.liftweb.http.rest.RestHelper
import net.liftweb.json._
import org.eordie.model.user.User
import net.liftmodules.mongoauth.LoginRedirect
import org.eordie.snippet.UserLogin
import net.liftweb.common.Full
import org.eordie.model.user.User.regUser
import org.eordie.config.Site

/**
 * Вход через сервис loginza
 *
 * @author Alexandr Kolosov
 * @since 11/12/12
 */
object LoginzaHandler extends Loggable with RestHelper {

  serve {
    case "loginza" :: Nil Post req => {
      val token = req.param("token").openTheBox
      logger.info("Received token: " + token)

      val url = "http://loginza.ru/api/authinfo?token=" + token
      val response = io.Source.fromInputStream(new java.net.URL(url).openStream()).mkString
      val userBox = processJson(parse(response))
      if (userBox.isDefined) {
        val user = userBox.openTheBox

        // no email in profile
        if (user.email.is.isEmpty) {
          logger.info("No email in user profile: " + user)
          S.notice("В Вашем профиле не указана электронная почта. Пожалуйста, заполните регистрационную форму")
          regUser(user)
          RedirectResponse(Site.register.fullUrl)
        } else {
          processUser(user)
          logger.info("Redirecting to home page")
          RedirectResponse(LoginRedirect.openOr(Site.home.url))
        }
      } else {
        S.notice("Не удалось получить данные профиля. Воспользуйтесь обычной формой для входа")
        RedirectResponse(Site.loginMenu.fullUrl)
      }
    }
  }

  def processUser(user: User) {
    User.findByEmail(user.email.is) match {
      case Full(dbUser) => UserLogin.logUserIn(dbUser, true)

      case _ => {
        logger.info("New user: " + user)
        user.save
        User.sendLoginToken(user)
        UserLogin.logUserIn(user, true)
        S.notice("Добро пожаловать! Дальнейшие инструкции высланы Вам на почту")
      }
    }
  }

  def processJson(json: JValue): Box[User] = {
    logger.info("Input json: " + json)

    json match {
      case JObject(List(_, _)) => {
        logger.info("Can't process token: " + json)
        Empty
      }

      case _ => {
        val result = json.extract[OK]
        val record: User = fillUser(result)

        Full(record)
      }
    }
  }

  def fillUser(result: LoginzaHandler.OK): User = {
    val record: User = User.createRecord
    if (result.email.isDefined) {
      record.email(result.email.get)
    }

    if (result.biography.isDefined) {
      record.bio(result.biography.get)
    }

    if (result.name.isDefined) {
      record.username(result.name.get.fullName)
    }

    if (result.phone.isDefined) {
      val mobile: String = result.phone.get.mobile
      val parsedMobile = parsePhone(mobile)
      record.mobilePhone(parsedMobile)
    }
    record
  }

  private def parsePhone(mobile: String): Long = {
    mobile.foldLeft(0l)((x: Long, char: Char) => if (char.isDigit) 10 * x + char.asDigit else x)
  }

  case class OK(email: Option[String], name: Option[Name], phone: Option[Phone], biography: Option[String])

  case class Name(first_name: String, last_name: String, full_name: Option[String]) {
    def fullName: String = {
      if (full_name.isDefined) full_name.get
      else first_name + " " + last_name
    }
  }

  case class Phone(preferred: String, home: String, work: String, mobile: String)
}
package org.eordie.model.user

import net.liftmodules.mongoauth.{AuthUtil, Permission, MongoAuthUser, ProtoAuthUser}
import net.liftweb.util.{FieldError, Helpers}
import net.liftweb.record.field.{BooleanField, EmailField, StringField}
import xml.Text
import net.liftmodules.mongoauth.field.{PermissionListField, PasswordField}
import net.liftweb.mongodb.record.field.StringRefListField
import net.liftmodules.mongoauth.model.Role

/**
 *
 * @author Alexandr Kolosov
 * @since 11/16/12
 */
trait LocalizedProtoAuthUser [T <: LocalizedProtoAuthUser[T]] extends MongoAuthUser[T] {

  self: T =>

  import Helpers._

  object username extends StringField(this, 32) {
    override def displayName = "Полное имя"
    override def setFilter = trim _ :: super.setFilter

    private def valUnique(msg: => String)(value: String): List[FieldError] = {
      if (value.length > 0)
        meta.findAll(name, value).filterNot(_.id.is == owner.id.is).map(u =>
          FieldError(this, Text(msg))
        )
      else
        Nil
    }

    override def validations =
      valUnique("Пользователь с таким именем уже существует. Выберите другое имя") _ ::
        valMinLen(3, "Имя пользователя должно быть длиннее трех символов") _ ::
        valMaxLen(32, "Имя пользователся должно быть короче 33 символов") _ ::
        super.validations
  }

  object email extends EmailField(this, 254) {
    override def displayName = "Электронная почта"
    override def setFilter = trim _ :: toLower _ :: super.setFilter

    private def valUnique(msg: => String)(value: String): List[FieldError] = {
      owner.meta.findAll(name, value).filter(_.id.is != owner.id.is).map(u =>
        FieldError(this, Text(msg))
      )
    }

    override def validations =
      valUnique("Данная электронная почта уже зарегистрирована. Если Вы забыли свой пароль, то перейдите на страницу входа") _  ::
        valMaxLen(254, "Длина электронноя почты должна быть короче 255 символов") _ ::
        super.validations
  }
  // email address has been verified by clicking on a LoginToken link
  object verified extends BooleanField(this) {
    override def displayName = "Подтвержден"
  }
  object password extends PasswordField(this, 6, 32) {
    override def displayName = "Пароль"
  }
  object permissions extends PermissionListField(this)
  object roles extends StringRefListField(this, Role) {
    def permissions: List[Permission] = objs.flatMap(_.permissions.is)
    def names: List[String] = objs.map(_.id.is)
  }

  lazy val authPermissions: Set[Permission] = (permissions.is ::: roles.permissions).toSet
  lazy val authRoles: Set[String] = roles.names.toSet

  lazy val fancyEmail = AuthUtil.fancyEmail(username.is, email.is)
}

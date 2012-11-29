package bootstrap.liftweb


import net.liftweb._
import common._
import http._
import util._

import org.eordie._
import config._
import lib.{RSSFeed, LoginzaHandler, UploadManager, Gravatar}
import model.user.{SystemUser, User}
import snippet.Notices

import net.liftmodules.mongoauth.MongoAuth

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Loggable {
  def boot() {
    logger.info("Run Mode: "+Props.mode.toString)

    // init mongodb
    MongoConfig.init()
    MongoConfig.clearUnusedFiles()

    // init auth-mongo
    MongoAuth.authUserMeta.default.set(User)
    MongoAuth.loginTokenAfterUrl.default.set(Site.password.url)
    MongoAuth.siteName.default.set("CarSpares")
    MongoAuth.systemEmail.default.set(SystemUser.user.email.is)
    MongoAuth.systemUsername.default.set(SystemUser.user.username.is)

    // For S.loggedIn_? and TestCond.loggedIn/Out builtin snippet
    LiftRules.loggedInTest = Full(() => User.isLoggedIn)

    // checks for ExtSession cookie
    LiftRules.earlyInStateful.append(User.testForExtSession)

    // Gravatar
    Gravatar.defaultImage.default.set("wavatar")

    // config an email sender
    SmtpMailer.init()

    // where to search snippet
    LiftRules.addToPackages("org.eordie")

    // set the default htmlProperties
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    // http://neuralmonkey.blogspot.com/2009/09/writing-view-page-in-lift.html
    LiftRules.statefulRewrite.append {
      case RewriteRequest(ParsePath(List("offers", "view", id),_,_,_),_,_) => RewriteResponse("offers" :: "view" :: Nil, Map("id" -> id))
      case RewriteRequest(ParsePath(List("offers", "edit", id),_,_,_),_,_) => RewriteResponse("offers" :: "edit" :: Nil, Map("id" -> id))
    }

    // Build SiteMap
    LiftRules.setSiteMap(Site.siteMap)

    // Error handler
    ErrorHandler.init()

    // 404 handler
    LiftRules.uriNotFound.prepend(NamedPF("404handler") {
      case (req, failure) =>
        NotFoundAsTemplate(ParsePath(List("404"), "html", false, false))
    })

    LiftRules.maxMimeFileSize = 40000000L
    LiftRules.maxMimeSize = 40000000L
    LiftRules.dispatch.append(UploadManager)

    // loginza oauth request
    LiftRules.dispatch.append(LoginzaHandler)

    LiftRules.dispatch.append(RSSFeed.dispatch)

    // Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-spinner").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-spinner").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    LiftRules.liftCoreResourceName = "lift-core_ru"

    LiftRules.resourceNames = "BasicMessages" :: "DetailMessages" :: Nil

    // Use custom code for notices
    Notices.init()
  }
}

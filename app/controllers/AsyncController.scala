package controllers

import akka.actor.ActorSystem
import javax.inject._
import play.api._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import play.api.libs.ws._
import play.api.libs.oauth._

/**
 * This controller creates an `Action` that demonstrates how to write
 * simple asynchronous code in a controller. It uses a timer to
 * asynchronously delay sending a response for 1 second.
 *
 * @param actorSystem We need the `ActorSystem`'s `Scheduler` to
 * run code after a delay.
 * @param exec We need an `ExecutionContext` to execute our
 * asynchronous code.
 */
@Singleton
class AsyncController @Inject() (actorSystem: ActorSystem, ws: WSClient)(implicit exec: ExecutionContext) extends Controller {

  /**
   * Create an Action that returns a plain text message after a delay
   * of 1 second.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/message`.
   */
  def message = Action.async {
    getFutureMessage(1.second).map { msg => Ok(msg) }
  }

  private def getFutureMessage(delayTime: FiniteDuration): Future[String] = {
    val promise: Promise[String] = Promise[String]()
    actorSystem.scheduler.scheduleOnce(delayTime) { promise.success("Hi!") }
    promise.future
  }
  
  def authTwitter = Action.async { implicit req =>
    val consumerKey = ConsumerKey("8*****9", "7*****W")
    val oauth = OAuth(ServiceInfo("https://api.twitter.com/oauth/request_token", "https://api.twitter.com/oauth/access_token",
      "https://api.twitter.com/oauth/authorize", consumerKey), true)

    req.getQueryString("oauth_verifier").map { verifier =>
      val tokenPair = sessionTokenPair(req).get
      play.api.Logger.info("token ----->" + tokenPair.token)
      play.api.Logger.info("secret ----->" + tokenPair.secret)
      oauth.retrieveAccessToken(tokenPair, verifier) match {
        case Right(t) => ws.url("https://api.twitter.com/1.1/search/tweets.json?q=%40twitterapi")
                        .get.map{ resp =>
                          play.api.Logger.info("API resp====>" + resp.body)
                          Redirect("/").withSession("token" -> t.token, "secret" -> t.secret)
                        }
        case Left(e) => throw e
      }
    }
    .getOrElse(
      oauth.retrieveRequestToken("http://localhost:9000/authTwitter") match {
        case Right(t) => Future.successful(Redirect(oauth.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret))
        case Left(e) => throw e
      }
    )
  }

  def authQBO = Action.async { implicit req =>
    val consumerKey = ConsumerKey("q*****D", "G*****b")
    val oauth = OAuth(ServiceInfo("https://oauth.intuit.com/oauth/v1/get_request_token", "https://oauth.intuit.com/oauth/v1/get_access_token",
      "https://appcenter.intuit.com/Connect/Begin", consumerKey), true)

    req.getQueryString("oauth_verifier").map { verifier =>
      val tokenPair = sessionTokenPair(req).get
      play.api.Logger.info("token ----->" + tokenPair.token)
      play.api.Logger.info("secret ----->" + tokenPair.secret)
      oauth.retrieveAccessToken(tokenPair, verifier) match {
        case Right(t) => ws.url("https://appcenter.intuit.com/api/v1/connection/reconnect")
                        .get.map{ resp =>
                          play.api.Logger.info("API resp====>" + resp.body)
                          Redirect("/").withSession("token" -> t.token, "secret" -> t.secret)
                        }
        case Left(e) => throw e
      }
    }
    .getOrElse(
      oauth.retrieveRequestToken("http://localhost:9000/authQBO") match {
        case Right(t) => Future.successful(Redirect(oauth.redirectUrl(t.token)).withSession("token" -> t.token, "secret" -> t.secret))
        case Left(e) => throw e
      }
    )
  }
  
  private def sessionTokenPair(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield RequestToken(token, secret)
  }

}

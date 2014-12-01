package uvm.utils

import scala.annotation.tailrec

object RetryUtils {

  /** Try a computation repeatedly until success. */
  @tailrec
  def tryRepeatedly[T](body: => Option[T]): T = {
    val maybeResult = body
    maybeResult match {
      case None => tryRepeatedly(body)
      case Some(v) => v
    }
  }

  def tryTwice[T](body: => Option[T]): T = {
    val maybeResult = body
    maybeResult match {
      case None => body.get
      case Some(v) => v
    }
  }

}
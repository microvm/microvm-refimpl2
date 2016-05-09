package uvm.refimpl.nat

import org.slf4j.LoggerFactory

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent

import ch.qos.logback.core.ConsoleAppender
import uvm.refimpl.MicroVM
import com.typesafe.scalalogging.Logger

object ScalaCInitiater {
  val logger = Logger(LoggerFactory.getLogger("priv.uvm.refimpl.native.ScalaCInitiater"))

  private var isLogConfigured = false;
    
  private def configureLog(): Unit = {
      if (isLogConfigured) {
          return
      }
      isLogConfigured = true
      
      val lc = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
      
      lc.reset()

      val ca = new ConsoleAppender[ILoggingEvent]()
      ca.setContext(lc)
      ca.setName("console")
      ca.setTarget("System.err")
      val pl = new PatternLayoutEncoder()
      pl.setContext(lc)
      pl.setPattern("%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n")
      pl.start()

      ca.setEncoder(pl)
      ca.start()
      val rootLogger = lc.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      rootLogger.addAppender(ca)
  }
 
  def mu_refimpl2_new(): Long = try {
    configureLog()
    val mvm = MicroVM()
    val fak = NativeClientSupport.exposeMicroVM(mvm)
    return fak
  } catch {
    case t: Throwable => {
      logger.error("Exception thrown when creating MicroVM", t)
      0L
    }
  }

  def mu_refimpl2_new_ex(vmConf: String): Long = try {
    val mvm = MicroVM(vmConf)
    val fak = NativeClientSupport.exposeMicroVM(mvm)
    return fak
  } catch {
    case t: Throwable => {
      logger.error("Exception thrown when creating MicroVM", t)
      0L
    }
  }

  def mu_refimpl_close(mvmFak: Long): Unit = try {
    NativeClientSupport.unexposeMicroVM(mvmFak)
  } catch {
    case t: Throwable => {
      logger.error("Exception thrown when closing MicroVM", t)
    }
  }
}
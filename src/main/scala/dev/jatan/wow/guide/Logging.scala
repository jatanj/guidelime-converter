package dev.jatan.wow.guide

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

trait Logging {
  @transient
  protected lazy val log: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

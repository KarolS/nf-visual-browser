package pl.umk.mat.stasiu88.nfclient.logging

trait Logging {
  
  def rawLog(msg: String):Unit
  
  def log_trace(msg: =>String) = rawLog("[TRACE] "+msg)
  def log_debug(msg: =>String) = rawLog("[DEBUG] "+msg)
  def log_info (msg: =>String) = rawLog("[INFO] " +msg)
  def log_warn (msg: =>String) = rawLog("[WARN] " +msg)
  def log_error(msg: =>String) = rawLog("[ERROR] "+msg)
  def log_fatal(msg: =>String) = rawLog("[FATAL] "+msg)
}

trait ConsoleLogging extends Logging {
  def rawLog(msg: String) = Console synchronized {
    // TODO: improve
    Console.out.println(this.getClass().getCanonicalName()+": "+msg) //TODO
    Console.out.flush()
  }
}

trait NullLogging extends Logging {
  def rawLog(msg: String) = ()
}
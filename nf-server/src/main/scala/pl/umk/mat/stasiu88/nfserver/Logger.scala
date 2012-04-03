package pl.umk.mat.stasiu88.nfserver

trait Logging {
  private def level = 0
  
  private def rawLog(msg: String) {
    println(this.getClass().getCanonicalName()+msg) //TODO
  }
  
  def log_trace(msg: =>String) = rawLog(": [TRACE] "+msg)
  def log_debug(msg: =>String) = rawLog(": [DEBUG] "+msg)
  def log_info (msg: =>String) = rawLog(": [INFO] " +msg)
  def log_error(msg: =>String) = rawLog(": [ERROR] "+msg)
  def log_fatal(msg: =>String) = rawLog(": [FATAL] "+msg)
}
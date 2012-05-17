package pl.umk.mat.stasiu88.nfclient.messages

sealed trait JobInfo{
  def id: Symbol
}

case class JobAccepted(id: Symbol) extends JobInfo

case class JobDone(id: Symbol) extends JobInfo

case class JobCancelled(id: Symbol) extends JobInfo

case class JobInterrupted(id: Symbol) extends JobInfo

case class JobError(id: Symbol, error: String) extends JobInfo

case class JobInProgress(id: Symbol) extends JobInfo

case class JobUnknown(id: Symbol) extends JobInfo

case object InvalidCredentials

case object ServerNotResponding

case object ServerBusy

case class ServerError(error: String)

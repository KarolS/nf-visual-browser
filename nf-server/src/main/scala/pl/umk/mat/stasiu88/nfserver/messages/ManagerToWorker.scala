package pl.umk.mat.stasiu88.nfserver.messages
import pl.umk.mat.stasiu88.nfserver.worker.Result
import scalaz._
import Scalaz._
case class StartNewJob(
    id: Symbol, 
    query: String
    )

case class CancelJob(
    id:Symbol
    )
    
case class ReportJobEnding(
    id: Symbol,
    result: Validation[Throwable, Result]
    )

case class ReportJobProgress(
    id: Symbol,
    progress: Double
    )
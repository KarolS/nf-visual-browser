package pl.umk.mat.stasiu88.nfclient.messages

case class SetCredentials(username: String, password: String)

case class SetServer(uri: String)

case class NewQuery(query: String)

case class CancelQuery(id: Symbol)

case class RefreshQuery(id: Symbol)

case object RefreshAllQueries
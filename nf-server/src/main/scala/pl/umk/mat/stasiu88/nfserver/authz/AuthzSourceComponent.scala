package pl.umk.mat.stasiu88.nfserver.authz

import pl.umk.mat.stasiu88.nfserver.messages.Credentials

trait AuthzSourceComponent{
  def authzSource: AuthzSource
}
trait TrivialAuthzSourceComponent{
  def authzSource = TrivialAuthzSource
}
trait AuthzSource{
  def authorize(credentials: Credentials):Boolean
}

object TrivialAuthzSource extends AuthzSource {
  def authorize(credentials: Credentials) = true
}
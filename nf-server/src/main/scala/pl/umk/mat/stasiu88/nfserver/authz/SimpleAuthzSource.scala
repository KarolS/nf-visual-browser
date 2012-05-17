package pl.umk.mat.stasiu88.nfserver.authz

import pl.umk.mat.stasiu88.nfserver.messages.Credentials

class SimpleAuthzSource(users: Set[Credentials]) extends AuthzSource{
  def authorize(credentials: Credentials) = users contains credentials
}
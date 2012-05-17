package pl.umk.mat.stasiu88.nfclient.serverconf

import pl.umk.mat.stasiu88.nfserver.configuration.ConfigurationComponent
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSourceComponent
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSource
import pl.umk.mat.stasiu88.nfserver.datasource.DataSourceComponent
import pl.umk.mat.stasiu88.nfserver.datasource.RandomDataSource
import pl.umk.mat.stasiu88.nfserver.messages.Credentials

trait TestingConfigurationComponent extends ConfigurationComponent 
                                    with    AuthzSourceComponent 
                                    with    DataSourceComponent{
  val authzSource = TestingConfiguration
  val dataSource = new RandomDataSource(1000) 
  lazy val httpPort = TestingConfiguration.httpPort
}

object TestingConfiguration extends AuthzSource{
  lazy val httpPort = 8889
  var allowed = Set[Credentials]()
  def authorize(credentials: Credentials) = allowed.contains(credentials)
}

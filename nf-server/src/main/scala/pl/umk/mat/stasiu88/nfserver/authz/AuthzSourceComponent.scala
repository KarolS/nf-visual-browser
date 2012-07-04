/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.authz

import pl.umk.mat.stasiu88.nfserver.messages.Credentials

/**
 * A composable trait for providing an authorization source.
 * <br>
 * Składalna cecha dostarczająca źródła autoryzacji.
 */
trait AuthzSourceComponent{
  def authzSource: AuthzSource
}
/**
 * A composable trait for providing a trivial authorization source.
 * <br>
 * Składalna cecha dostarczająca trywialnego źródła autoryzacji.
 */
trait TrivialAuthzSourceComponent{
  def authzSource = TrivialAuthzSource
}
/**
 * Authorization source, used for accepting or rejecting credentials.
 * <br>
 * Źródło autoryzacji, akceptujące lub odrzucające uwierzytelnienia.
 */
trait AuthzSource{
  /**
   * Returns <code>true</code> if the credentials are correct.
   * <br>
   * Zwraca <code>true</code>, jeśli uwierzytelnienie jest poprawne.
   */
  def authorize(credentials: Credentials):Boolean
}

/**
 * Trivial authorization source, accepting all credentials.
 * <br>
 * Trywialne źródło autoryzacji, akceptujące wszystkie uwierzytelnienia.
 */
object TrivialAuthzSource extends AuthzSource {
  def authorize(credentials: Credentials) = true
}
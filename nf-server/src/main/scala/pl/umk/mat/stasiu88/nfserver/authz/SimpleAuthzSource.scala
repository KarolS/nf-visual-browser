/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.authz

import pl.umk.mat.stasiu88.nfserver.messages.Credentials

/**
 * Authorization source accepting only credentials from the set.
 * <br>
 * Źródło autoryzacji akceptujące tylko uwierzytelnienia ze zbioru.
 */
class SimpleAuthzSource(users: Set[Credentials]) extends AuthzSource{
  def authorize(credentials: Credentials) = users contains credentials
}
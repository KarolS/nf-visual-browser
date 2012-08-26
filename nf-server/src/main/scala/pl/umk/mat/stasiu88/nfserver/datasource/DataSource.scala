/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.datasource

import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.worker.MutableResult

/**
 * A composable trait for providing data source.
 * <br>
 * Składalna cecha dostarczająca źródło danych.
 */
trait DataSourceComponent {
  def dataSource: DataSource
}

/**
 * Iterates through and summarized flow data.
 * <br>
 * Iteruje po i przetwarza dane o przepływach.
 */
trait DataSource {
  
  /**
   * Calls a function for each flow record in this source.
   * The flow record is a mutable object, so the implementation is free to reuse the same object.
   * <br>
   * Wywołuje funkcję dla każdego rekordu z przepływem w tym źródle.
   * Rekord jest mutowalny, więc implementacja może używać wielokrotnie tego samego obiektu.
   */
  def foreach(query:Query)(f:Flow=>Unit): Unit
  
  /**
   * Executes a query and returns a result of that query on flow data in this source.
   * Parameter <code>approxThreadCount</code> specifies desired approximate number of threads, 
   * but there don't have to be any guarantees.  
   * Parameter <code>reportProgress</code> specifies a callback for reporting progress on processing the data.
   * Argument to <code>reportProgress</code> is from range [0-1].
   * <br>
   * Wykonuje zapytanie i zwraca jego wyniki na danych o przepływach z tego źródła.
   * Parametr <code>approxThreadCount</code> to żądana przybliżona liczba wątków,
   * ale nie musi być żadnych gwarancji.
   * Parametr <code>reportProgress</code> to funkcja, która będzie wywoływana w celu raportowania postępu przetwarzania danych.
   * Argument do <code>reportProgress</code> jest z przedziału [0-1].
   */
  def getResult(q:Query, approxThreadCount: Int=8)(reportProgress: Double=>Unit): Result = getResultSingleThreaded(q)(reportProgress)
  
  /**
   * Tries to provide a result in a faster way for some simple queries.
   * <br>
   * Próbuje dostarczyć odpowiedzi w szybszy sposób dla pewnych prostych zapytań.  
   */
  def getQuickResult(q:Query): Option[Result]= None
  
  /**
   * Executes a query and returns a result, using only a one thread.
   * <br>
   * Wykonuje zapytanie i zwraca jego wyniki, używając tylko jednego wątku.
   */
  def getResultSingleThreaded(q:Query)(reportProgress: Double=>Unit): Result = {
    getQuickResult(q) foreach { return _ }
    val result = new MutableResult(q.splitfilter.bucketCount, q.statistic.sumOver.length)
    foreach(q){ f=>
      val indices = q.statistic.indexing(q,f)
      val period = q.statistic.period.apply(q,f)
      q.splitfilter.classify(f) foreach { bucket =>
        var i = 0
        for(v <- q.statistic.sumOver){
          for(index<-indices){
            result.add(bucket,i,period,index,v(f))
          }
          i += 1
        }
      }
    }
    val frozenResult = result.freeze()
    reportProgress(1.0)
    frozenResult
  }
}

package pl.umk.mat.stasiu88.nfclient

object App {
  def main(args: Array[String]) ={
    val c = new SHttpClient()
    val (status1, result1) = c.request("http://localhost/post_test.php", Map("a"->"b"))()
    val (status2, result2) = c.request("http://localhost/post_test.php", Map("a"->"b"))(_ => true)
    val (status3, result3) = c.request("http://localhost/pony.html", Map("a"->"b"))()
    val (status4, result4) = c.request("http://localhost/pony.html", Map("a"->"b"))(_ => true)
    println(result1)
    println(result2)
    println(result3)
    println(result4)
  }
}
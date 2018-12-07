package nodescala

import java.util.Date

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  test("A list of future should be completed if all the futures in it completes") {
    val res = Future.all(List(Future{math.pow(2.0,34.0)}, Future{2.0 / 1.0}))

    res.onComplete {
      case Success(e) => assert(true)
      case _ => assert(false)
    }
  }
  test("A list of future should be failed if at least one of the futures in it fails") {
    //divide by zero
    val res = Future.all(List(Future{math.pow(2.0,34.0)}, Future{2.0 / 0.0}))

    res.onComplete {
      case Success(e) => assert(false)
      case _ => assert(true)
    }
  }
  test("A list of future should be completed if the first that completes completes with success") {
    //divide by zero with wait in order to complete the first Future instead
    val res = Future.any(List(Future{math.pow(2.0,34.0)}, Future{Thread.sleep(1000); 2.0 / 0.0}))

    res.onComplete {
      case Success(e) => assert(true)
      case _ => assert(false)
    }
  }
  ignore("A delay must be punctual") {
    Try(Await.result(Future.delay(100 millis), 101 millis)) match {
      case Success(x) => assert(true)
      case Failure(ex) => assert(false)
    }
  }
  //The behaviour of this test is not guranteed since sometimes the now method is called earlier then the Future finishes
  /*test("The Future's now method should be completed when it is called") {
    Try(Future(1).now) match {
      case Success(e) => assert(true)
      case _ => assert(false)
    }
  }*/
  test("The Future's now method should be thrown NoSuchElementException when the expression cannot be completed directly after call") {
    Try(Future.delay(10 millis).now) match {
      case Failure(ex: NoSuchElementException) => assert(true)
      case _ => assert(false)
    }
  }
  ignore("The Future's run method") {
    var start: Long = 0
    var done: Long = 0
    val working = Future.run() { ct =>
      Future {
        start = System.currentTimeMillis()
        while (ct.nonCancelled) {
          ;
        }
        done = System.currentTimeMillis()
      }
    }
    Future.delay(10 millis) onSuccess {
      case _ => working.unsubscribe()
    }
    assert((start - done) > (10 millis).toMillis)
  }

  
  
  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  ignore("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }
  test("Server should be stoppable if receives infinite  response") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => Iterator.continually("a")
    }

    // wait until server is really installed
    Thread.sleep(500)

    val webpage = dummy.emit("/testDir", Map("Any" -> List("thing")))
    try {
      // let's wait some time
      Await.result(webpage.loaded.future, 1 second)
      fail("infinite response ended")
    } catch {
      case e: TimeoutException =>
    }

    // stop everything
    dummySubscription.unsubscribe()
    Thread.sleep(500)
    webpage.loaded.future.now // should not get NoSuchElementException
  }

}





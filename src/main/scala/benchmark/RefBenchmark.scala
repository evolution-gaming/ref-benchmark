package benchmark

import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.openjdk.jmh.annotations.Benchmark

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

import RefBenchmark._

class RefBenchmark {

  def run(f: => IO[A]): Unit =
    Sync[IO].replicateA_(100000, f).unsafeRunSync()

  @Benchmark
  def strongGetAndUpdate(): Unit =
    run(RefBenchmark.strongGetAndUpdate(_.inc))

  @Benchmark
  def weakGetAndUpdate(): Unit =
    run(RefBenchmark.weakGetAndUpdate(_.inc))

  @Benchmark
  def javaGetAndUpdate(): Unit =
    run(RefBenchmark.javaGetAndUpdate(_.inc))

}
object RefBenchmark {

  type A = Wrapper

  case class Wrapper(value: Int) {
    def inc: Wrapper = this.copy(value = value + 1)
  }

  val ar: AtomicReference[Wrapper] = new AtomicReference(Wrapper(0));

  def strongGetAndUpdate(f: A => A): IO[A] = {
    @tailrec
    def spin: A = {
      val a = ar.get
      val u = f(a)
      if (!ar.compareAndSet(a, u)) spin
      else a
    }
    IO.delay(spin)
  }

  def weakGetAndUpdate(f: A => A): IO[A] = {
    @tailrec
    def spin: A = {
      val a = ar.get
      val u = f(a)
      if (!ar.weakCompareAndSetVolatile(a, u)) spin
      else a
    }
    IO.delay(spin)
  }

  def javaGetAndUpdate(f: A => A): IO[A] =
    IO.delay(ar.getAndUpdate(a => f(a)))

}

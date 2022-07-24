package com.rockthejvm.part2effects

import zio._

import scala.io.StdIn

object ZIOEffects {

  // success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  // failure
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  // suspension/delay
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  // map + flatMap
  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
  // for comprehensions
  val smallProgram = for {
    _ <- ZIO.succeed(println("what's your name"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  // A LOT of combinators
  // zip, zipWith
  val anotherMOL = ZIO.succeed(100)
  val tupledZIO = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /**
   * Type aliases of ZIOs
   */
  // UIO[A] = ZIO[Any,Nothing,A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)
  // URIO[R,A] = ZIO[R,Nothing,A] - cannot fail
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)
  // RIO[R,A] = ZIO[R,Throwable, A] - can fail with a Throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))
  // Task[A] = ZIO[Any, Throwable, A] - no requirements, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))
  // IO[E,A] = ZIO[Any,E,A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")

  /**
   * Exercices
   */

  // 1
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      va <- zioa
      vb <- ziob
    } yield vb

  // 2
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    for {
      va <- zioa
      vb <- ziob
    } yield va

  // 3
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.flatMap(_ => runForever(zio))

  val endlessLoop = runForever {
    ZIO.succeed {
      println("running...")
      //Thread.sleep(1000)
    }
  }

  // 4
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    for {
      _ <- zio
    } yield value

  // 5
  def discard[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = convert(zio, ())

  // 6
  def sum(n: Int): Int =
    if n == 0 then 0 else n + sum(n - 1)

  def sumZIO(n: Int): UIO[Int] =
    if n == 0 then ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      next <- sumZIO(n - 1)
    } yield current + next

  // 7
  // hint: use ZIO.suspend/ZIO.suspendSucceed
  def fiboZIO(n: Int): UIO[BigInt] =
    if n <= 1 then ZIO.succeed(n)
    else for {
      _ <- ZIO.succeed(314)
      n_1 <- fiboZIO(n - 1)
      n_2 <- fiboZIO(n - 2)
    } yield n_1 + n_2

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty
    Unsafe.unsafeCompat { implicit u =>
      println(runtime.unsafe.run(sequenceTakeFirst(ZIO.succeed(1), ZIO.succeed(2))))
      println(runtime.unsafe.run(sumZIO(3)))
      println(runtime.unsafe.run(fiboZIO(12)))
    }
  }
}

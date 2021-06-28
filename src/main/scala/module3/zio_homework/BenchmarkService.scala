package module3.zio_homework

import zio.clock.Clock
import zio.console.Console
import zio.{UIO, ZIO}
import zio._

import java.util.concurrent.TimeUnit

trait BenchmarkService {
  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A]
}

object BenchmarkService {
  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Has[BenchmarkService], E, A] =
    ZIO.service[BenchmarkService].flatMap(_.printEffectRunningTime(zio))
}

final case class BenchmarkServiceLive(console: Console.Service, clock: Clock.Service) extends BenchmarkService {
  val currentTime: UIO[Long] = clock.currentTime(TimeUnit.SECONDS)

  override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for {
    start <- currentTime
    res   <- zio
    end   <- currentTime
    _     <- console.putStrLn(s"Running time ${end - start}")
  } yield res
}

object BenchmarkServiceLive {
  val layer: URLayer[Has[Console.Service] with Has[Clock.Service], Has[BenchmarkService]] = {
    ZLayer.fromEffect(
      for {
        console <- ZIO.service[Console.Service]
        clock   <- ZIO.service[Clock.Service]
      } yield BenchmarkServiceLive(console, clock)
    )
  }
}

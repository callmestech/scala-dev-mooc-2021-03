package module3.zio_homework
import zio.{ExitCode, URIO}

object Application extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    appSpeedUp
      .provideCustomLayer(BenchmarkServiceLive.layer)
      .exitCode
}

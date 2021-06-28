package module3

import module3.zio_homework.config.AppConfig
import zio.clock.Clock
import zio.console.{Console, getStrLn}
import zio.duration.durationInt
import zio.random.Random

import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет.
   */

  import zio._

  lazy val guessProgram: RIO[Console with Random, Unit] = (for {
    _     <- console.putStrLn("Hey! Try to guess a number between 1 and 3")
    _     <- console.putStrLn("Enter a number >")
    input <- console.getStrLn >>= validateInput
    n      <- random.nextIntBetween(1, 4)
    _ <-
      if (input == n) console.putStrLn("You got it!")
      else console.putStrLn(s"You wrong! $n is right answer")

    eff = console.putStrLn("Do you want to try once again? `y` or `n`") *> getStrLn

    onceAgain <- eff
    _ <-
      if (onceAgain.equalsIgnoreCase("y")) guessProgram
      else if (onceAgain.equalsIgnoreCase("n")) console.putStrLn("Bye!")
      else console.putStrLn("Can't understand your answer") *> eff

  } yield ()).orElse(guessProgram)

  private def validateInput(input: String) =
    ZIO
      .effect(input.toInt)
      .tapError(_ => console.putStrLn(s"Error! Only numbers input is allowed, but you entered `$input`."))
      .filterOrFail(n => n >= 1 && n < 4)(new IllegalArgumentException("Number should be be between 1 and 3"))
      .tapError(e => console.putStrLn(e.getMessage))

  /**
   * 2. реализовать функцию doWhile, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   */

  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    body.repeatWhile(condition)

  def doWhile2[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    body.filterOrElse(a => !condition(a))(_ => doWhile(body)(condition))

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault(default: => AppConfig): RIO[Console, AppConfig] =
    config
      .load
      .orElseSucceed(default)
      .tap(cfg => console.putStrLn(s"App name - ${cfg.appName}, app url - ${cfg.appUrl}"))

  val loadConfig: RIO[Console, AppConfig] = loadConfigOrDefault(config.default)

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  val eff: URIO[Random with Clock, Int] = random.nextIntBetween(0, 11).delay(1.second)
  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  val effects: Seq[URIO[Random with Clock, Int]] = Seq.fill(10)(eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */
  val app: URIO[Env, Int] =
    composeSumEffect(ZIO.collectAll(_))

  type Env = Random with Clock with Console with Has[BenchmarkService]

  private def composeSumEffect(f: Seq[URIO[Env, Int]] => URIO[Env, Seq[Int]]): URIO[Env, Int] =
    BenchmarkService.printEffectRunningTime(
      for {
        numbers <- f(effects)
        sum     = numbers.sum
        _       <- console.putStrLn(s"Sum of elements: $sum")
      } yield sum
    )

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val appSpeedUp: URIO[Env, Int] = composeSumEffect(ZIO.collectAllPar(_))

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */
  //look at BenchmarkService
}

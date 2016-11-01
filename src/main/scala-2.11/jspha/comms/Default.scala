package jspha.comms

/**
  * In order to not require passing parameters to every query we allow for
  * defaults to be specified. These must be available and identical on client
  * and server, so it's best to be careful in using them.
  */
trait DefaultParam[T] {
  val value: T
}

object DefaultParam {

  def apply[T](t: T): DefaultParam[T] = new DefaultParam[T] {
    val value: T = t
  }

  implicit val DefaultParamUnit: DefaultParam[Unit] =
    apply(())

  implicit def DefaultParamOption[A]: DefaultParam[Option[A]] =
    apply(None)

}


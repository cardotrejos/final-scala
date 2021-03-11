package co.s4ncampus.fpwithscala.users.domain

import cats.Applicative
import cats.data.EitherT
import cats.syntax.all._

class UserValidationInterpreter[F[_]: Applicative](repository: UserRepositoryAlgebra[F])
    extends UserValidationAlgebra[F] {

  def doesNotExist(user: User): EitherT[F, UserAlreadyExistsError, Unit] = 
    repository.findByLegalId(user.legalId).map(UserAlreadyExistsError).toLeft(())

  def exists(userId: Option[String]): EitherT[F, UserNotFoundError.type, Unit] =
    userId match {
      case Some(id) =>
        repository
          .findByLegalId(id)
          .toRight(UserNotFoundError)
          .void
      case None =>
        EitherT.left[Unit](UserNotFoundError.pure[F])
    }

}

object UserValidationInterpreter {
  def apply[F[_]: Applicative](repository: UserRepositoryAlgebra[F]) =
    new UserValidationInterpreter[F](repository)
}
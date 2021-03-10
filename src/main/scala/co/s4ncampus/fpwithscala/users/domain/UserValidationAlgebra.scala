package co.s4ncampus.fpwithscala.users.domain

import cats.data.EitherT

trait UserValidationAlgebra[F[_]] {
  /* Fails with a UserAlreadyExistsError */
  def doesNotExist(user: User): EitherT[F, UserAlreadyExistsError, Unit]
  def doesNotUserExist(legalId: String): EitherT[F,UserDoesntExistsError, Unit]
}
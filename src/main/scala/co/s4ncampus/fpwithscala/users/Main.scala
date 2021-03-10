package co.s4ncampus.fpwithscala.users


import co.s4ncampus.fpwithscala.users.config._
import co.s4ncampus.fpwithscala.users.domain._
import co.s4ncampus.fpwithscala.users.controller._
import co.s4ncampus.fpwithscala.users.infraestructure.repository._
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.effect.ContextShift
import cats.effect.ConcurrentEffect
import cats.effect.Timer
import cats.effect.Resource
import org.http4s.server.Server
import io.circe.config.parser
import org.http4s.server.blaze.BlazeServerBuilder
import cats.effect.Blocker
import doobie.util.ExecutionContexts
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router

object Main extends IOApp{

    private def server[F[_]: ContextShift: ConcurrentEffect: Timer]: Resource[F, Server[F]] = 
        for {
            conf <- Resource.liftF(parser.decodePathF[F, UsersConfig]("users"))
            serverExecutionContexts <- ExecutionContexts.cachedThreadPool[F]
            connectionExecutionContext <- ExecutionContexts.fixedThreadPool[F](conf.db.connections.poolSize)
            transactionsExecutionContext <- ExecutionContexts.cachedThreadPool[F]
            xa <- DatabaseConfig.dbTransactor(conf.db, connectionExecutionContext, Blocker.liftExecutionContext(transactionsExecutionContext))
            userRepo = DoobieUserRepositoryInterpreter[F](xa)
            userValidation = UserValidationInterpreter[F](userRepo)
            userService = UserService[F](userRepo, userValidation)
            httpApp = Router(
                "/users" -> UsersController.endpoints[F](userService)
            ).orNotFound
            _ <- Resource.liftF(DatabaseConfig.initializeDb(conf.db))
            server <- BlazeServerBuilder[F](serverExecutionContexts)
                .bindHttp(conf.server.port, conf.server.host)
                .withHttpApp(httpApp)
                .resource
        } yield server

    def run(args: List[String]): IO[ExitCode] = server.use(_ => IO.never).as(ExitCode.Success)
}

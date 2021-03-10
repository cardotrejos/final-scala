val CatsVersion = "2.2.0"
val Http4sVersion = "0.21.16"
val CirceVersion = "0.13.0"
val CirceConfigVersion = "0.8.0"
val CirceGenericExtrasVersion = "0.13.0"
val DoobieVersion = "0.9.2"
val H2Version = "1.4.200"
val MunitVersion = "0.7.20"
val LogbackVersion = "1.2.3"
val MunitCatsEffectVersion = "0.13.0"
val FlywayVersion = "7.2.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.s4ncampus",
    name := "users",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.0",
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-core"            % CatsVersion,
      "org.http4s"      %% "http4s-blaze-server"  % Http4sVersion,
      "org.http4s"      %% "http4s-blaze-client"  % Http4sVersion,
      "org.http4s"      %% "http4s-circe"         % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"           % Http4sVersion,
      "io.circe"        %% "circe-core"           % CirceVersion,
      "io.circe"        %% "circe-generic"        % CirceVersion,
      "io.circe"        %% "circe-parser"         % CirceVersion,
      "io.circe"        %% "circe-config"         % CirceConfigVersion,
      "io.circe"        %% "circe-generic-extras" % CirceGenericExtrasVersion,
      "org.tpolecat"    %% "doobie-core"          % DoobieVersion,
      "org.tpolecat"    %% "doobie-h2"            % DoobieVersion,
      "org.tpolecat"    %% "doobie-hikari"        % DoobieVersion,
      "org.tpolecat"    %% "doobie-postgres"      % DoobieVersion,
      "com.h2database"  %  "h2"                   % H2Version,
      "org.flywaydb"    %  "flyway-core"          % FlywayVersion,
      "org.scalameta"   %% "munit"                % MunitVersion           % Test,
      "org.typelevel"   %% "munit-cats-effect-2"  % MunitCatsEffectVersion % Test,
      "org.tpolecat"    %% "doobie-scalatest"     % DoobieVersion          % Test,
      "ch.qos.logback"  %  "logback-classic"      % LogbackVersion,
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
    testFrameworks += new TestFramework("munit.Framework")
  )

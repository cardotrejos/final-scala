package co.s4ncampus.fpwithscala.users.config

final case class ServerConfig(host: String, port: Int)
final case class UsersConfig(db: DatabaseConfig, server: ServerConfig)
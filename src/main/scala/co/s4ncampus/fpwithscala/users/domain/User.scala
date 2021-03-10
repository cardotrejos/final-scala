package co.s4ncampus.fpwithscala.users.domain

case class User(
    id: Option[Long],
    legalId: String,
    firstName: String,
    lastName: String,
    email: String,
    phone: String)
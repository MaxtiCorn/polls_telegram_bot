package domain

import java.time.LocalDateTime

import info.mukel.telegrambot4s.models.User

import scala.util.{Failure, Success, Try}

case class Poll(user: User, pollName: String,
                anon: Boolean = true, visible: Boolean = true,
                startDate: Option[LocalDateTime] = None, endDate: Option[LocalDateTime] = None,
                questions: Vector[Question] = Vector.empty) {

  override def toString: String = {
    s"poll name: $pollName" +
      s"\nanonymity: ${anon.toString}" +
      s"\nvisible: ${visible.toString}" +
      s"\nstart date: ${startDate.getOrElse("none").toString}" +
      s"\nend date: ${startDate.getOrElse("none").toString}" +
      s"\nquestions:\n${
        questions
          .map(q => q.toString)
          .mkString("\n")
      }"
  }

  def result: String = questions
    .map(q => q.stats)
    .mkString("\n")

  def start: Poll = copy(startDate = Option(LocalDateTime.now()))

  def stop: Poll = copy(endDate = Option(LocalDateTime.now()))

  def started: Boolean = {
    startDate match {
      case Some(timeToStart) =>
        endDate match {
          case Some(timeToStop) => timeToStart.isBefore(LocalDateTime.now()) && timeToStop.isAfter(LocalDateTime.now())
          case None => timeToStart.isBefore(LocalDateTime.now())
        }
      case None => false
    }
  }

  def canEdit(u: User): Boolean = u.id == user.id

  def addQuestion(question: Question): Poll = copy(questions = questions :+ question)

  def deleteQuestion(number: Int): Poll = copy(questions = questions.filter(q => !q.equals(questions.apply(number))))

  def addAnswers(questionIndex: Int, user: User, answers: Vector[String]): Poll = {
    copy(questions = questions.map(
      q => Try(require(questions.indexOf(q).equals(questionIndex))) match {
        case Success(_) => q.addAnswer(user, answers)
        case Failure(_) => q
      }
    ))
  }
}
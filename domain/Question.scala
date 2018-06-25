package domain

import info.mukel.telegrambot4s.models.User

case class Question(question: String, questionType: String = "open",
                    variants: Vector[String] = Vector.empty,
                    answers: Map[User, Vector[String]] = Map.empty) {
  override def toString: String = {
    s"$question $questionType\n ${variants.mkString("\n")}"
  }

  def stats: String = {
    questionType match {
      case "open" => answers
        .map(p => p._1.username.getOrElse("") + " " + p._2.mkString(" "))
        .mkString("\n")
      case "multi" => variants.map(v => (v, answers.values.count(a => a.contains(v))))
        .map(p => p._1 + ": " + p._2.toString)
        .mkString("\n")
      case "choice" => variants.map(v => (v, answers.values.count(a => a.contains(v))))
        .map(p => p._1 + ": " + p._2.toString)
        .mkString("\n")
    }
  }

  def addAnswer(user: User, userAnswers: Vector[String]): Question = {
    questionType match {
      case "open" => copy(answers = answers + (user -> userAnswers))
      case "multi" => copy(answers = answers + (user -> userAnswers.map(a => variants.apply(a.toInt))))
      case "choice" => copy(answers = answers + (user -> userAnswers.map(a => variants.apply(a.toInt))))
    }
  }
}

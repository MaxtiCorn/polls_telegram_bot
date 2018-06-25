package ui

import java.time.LocalDateTime

import domain.{Parser, Poll, Question}
import info.mukel.telegrambot4s.models.User
import repository.PollsRepository

import scala.util.{Failure, Success, Try}

class CommandExecutor {
  var context: Option[Int] = None

  def executeCommand(command: String, user: Option[User]): String = {
    val parser = new Parser
    val parsedCommand = parser.parse(command)
    val commandName = parsedCommand.head
    val args = parsedCommand.tail
    commandName match {
      case "/list" => list(args)
      case "/create_poll" => createPoll(args, user.get)
      case "/delete_poll" => deletePoll(args, user.get)
      case "/start_poll" => startPoll(args, user.get)
      case "/stop_poll" => stopPoll(args, user.get)
      case "/result" => result(args)
      case "/begin" => begin(args)

      case "/view" => view()
      case "/add_question" => addQuestion(args, user.get)
      case "/delete_question" => deleteQuestion(args, user.get)
      case "/answer" => answer(args, user.get)
      case "/end" => end()
      case _ => "unknown command"
    }
  }

  def list(args: Seq[String]): String = {
    PollsRepository.getAll
      .map(entry => entry._1.toString + ' ' + entry._2.pollName)
      .reduce(_ + "\n" + _)
  }

  def createPoll(args: Seq[String], user: User): String = {
    val name = args.lift(0) getOrElse ""
    Try(require(name != "")) match {
      case Failure(_) => return "fail: need poll name"
      case Success(_) =>
    }
    val anon = args.lift(1) match {
      case Some(value) => value.contains("yes")
      case None => true
    }
    val visible = args.lift(2) match {
      case Some(value) => value.contains("continuous")
      case None => false
    }
    val startDate = args.lift(3) match {
      case Some(value) => Some(LocalDateTime.parse(value))
      case None => None
    }
    val endDate = args.lift(4) match {
      case Some(value) => Some(LocalDateTime.parse(value))
      case None => None
    }
    val poll = Poll(user, name, anon, visible, startDate, endDate)
    PollsRepository.add(poll).toString
  }

  def deletePoll(args: Seq[String], user: User): String = {
    val pollId = args.lift(0) getOrElse ""
    Try(require(pollId != "")) match {
      case Failure(_) => return "fail: need poll id"
      case Success(_) =>
    }
    PollsRepository.get(pollId.toInt) match {
      case Some(value) => Try(require(value.canEdit(user))) match {
        case Failure(_) => return "fail"
        case Success(_) =>
      }
      case None => return "fail"
    }
    Try(require(PollsRepository.delete(pollId.toInt))) match {
      case Success(_) => "success"
      case Failure(_) => "fail"
    }
  }

  def startPoll(args: Seq[String], user: User): String = {
    val id = args.lift(0) match {
      case Some(value) => Try(value.toInt) match {
        case Success(i) => i
        case Failure(_) => return "fail: incorrect id"
      }
      case None => return "fail: need poll id"
    }
    val poll = PollsRepository.get(id) match {
      case Some(value) => Try(require(value.canEdit(user))) match {
        case Failure(_) => return "fail"
        case Success(_) => value
      }
      case None => return "fail: incorrect id"
    }
    Try(require(!poll.started)) match {
      case Success(_) => PollsRepository.update(id, poll.start)
        "success"
      case Failure(_) => "fail: poll already started"
    }
  }

  def stopPoll(args: Seq[String], user: User): String = {
    val id = args.lift(0) match {
      case Some(value) => Try(value.toInt) match {
        case Success(i) => i
        case Failure(_) => return "fail: incorrect id"
      }
      case None => return "fail: need poll id"
    }
    val poll = PollsRepository.get(id) match {
      case Some(value) => Try(require(value.canEdit(user))) match {
        case Failure(_) => return "fail"
        case Success(_) => value
      }
      case None => return "fail: incorrect id"
    }
    Try(require(poll.started)) match {
      case Success(_) => PollsRepository.update(id, poll.stop)
        "success"
      case Failure(_) => "fail: poll already stopped"
    }
  }

  def result(args: Seq[String]): String = {
    val id = args.lift(0) match {
      case Some(value) => Try(value.toInt) match {
        case Success(i) => i
        case Failure(_) => return "fail: incorrect id"
      }
      case None => return "fail: need poll id"
    }
    val poll = PollsRepository.get(id) match {
      case Some(value) => value
      case None => return "fail: incorrect id"
    }
    Try(require(poll.started && poll.visible || !poll.started)) match {
      case Success(_) => poll.result
      case Failure(_) => "fail"
    }
  }

  def begin(args: Seq[String]): String = {
    args.lift(0) match {
      case Some(value) => Try(value.toInt) match {
        case Success(i) => Try(require(PollsRepository.contains(i))) match {
          case Success(_) => context = Some(i)
            "success"
          case Failure(_) => "fail: incorrect id"
        }
        case Failure(_) => "fail: incorrect id"
      }
      case None => "fail: need id"
    }
  }


  def view(): String = {
    context match {
      case Some(value) => PollsRepository.get(value).get.toString
      case None => "fail: need context"
    }
  }

  def addQuestion(args: Seq[String], user: User): String = {
    val poll = context match {
      case Some(value) => PollsRepository.get(value).get
      case None => return "fail: need context"
    }
    Try(require(poll.canEdit(user))) match {
      case Failure(_) => return "fail: you can't add question"
      case Success(_) =>
    }
    val questionText = args.lift(0) getOrElse ""
    Try(require(questionText != "")) match {
      case Failure(_) => return "fail: need question"
      case Success(_) =>
    }
    args.lift(1) match {
      case Some(value) => Try(assert(List("open", "choice", "multi").contains(value))) match {
        case Success(_) => val question = Question(questionText, value, args.drop(2).toVector)
          PollsRepository.update(context.get, poll.addQuestion(question))
          "success"
        case Failure(_) => val question = Question(questionText, "open", args.drop(1).toVector)
          PollsRepository.update(context.get, poll.addQuestion(question))
          "success"
      }
      case None => val question = Question(questionText)
        PollsRepository.update(context.get, poll.addQuestion(question))
        "success"
    }
  }

  def deleteQuestion(args: Seq[String], user: User): String = {
    val poll = context match {
      case Some(value) => PollsRepository.get(value).get
      case None => return "fail: need context"
    }
    Try(require(poll.canEdit(user))) match {
      case Failure(_) => return "fail"
      case Success(_) =>
    }
    val qNum = args.lift(0) getOrElse ""
    Try(require(qNum != "")) match {
      case Failure(_) => return "fail: need question number"
      case Success(_) =>
    }
    Try(require(poll.questions.lift(qNum.toInt).getOrElse("") != "")) match {
      case Success(_) => PollsRepository.update(context.get, poll.deleteQuestion(qNum.toInt))
        "success"
      case Failure(_) => "fail"
    }
  }

  def answer(args: Seq[String], user: User): String = {
    val poll = context match {
      case Some(value) => PollsRepository.get(value).get
      case None => return "fail: need context"
    }
    val inputIndex = args.lift(0) getOrElse ""
    Try(require(inputIndex != "")) match {
      case Failure(_) => return "fail: need question number"
      case Success(_) =>
    }
    val answers = args.drop(1)
    Try(inputIndex.toInt) match {
      case Success(i) => poll.questions.lift(i) match {
        case Some(_) =>
          Try(require(!poll.questions.apply(i).answers.contains(user))) match {
            case Success(_) => PollsRepository.update (context.get, poll.addAnswers (i, user, answers.toVector) )
          "success"
            case Failure(_) => "fail: already answered"
          }
        case None => "fail: incorrect question index"
      }
      case Failure(_) => "fail: incorrect question index"
    }
  }

  def end(): String = {
    context match {
      case Some(_) => context = None
        "success"
      case None => "fail: need context"
    }
  }
}

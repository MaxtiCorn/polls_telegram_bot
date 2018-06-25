package repository

import domain.Poll

import scala.util.{Failure, Success, Try}

object PollsRepository {
  private var db: Map[Int, Poll] = Map.empty

  def contains(id: Int): Boolean = {
    db.filterKeys(i => i == id).nonEmpty
  }

  def get(id: Int): Option[Poll] = {
    db.get(id)
  }

  def add(poll: Poll): Int = {
    val id = db.keySet.size match {
      case 0 => 0
      case _ => db.keySet.max + 1
    }
    db += (id -> poll)
    id
  }

  def update(id: Int, poll: Poll): Unit = {
    db += (id -> poll)
  }

  def delete(id: Int): Boolean = {
    val assertExists = Try(assert(db.filterKeys(key => key == id).nonEmpty))
    assertExists match {
      case Success(_) => val assertNotStarted = Try(assert(!db(id).started))
        assertNotStarted match {
          case Success(_) => db -= id
            true
          case Failure(_) => false
        }
      case Failure(_) => false
    }
  }

  def getAll: Map[Int, Poll] = db
}

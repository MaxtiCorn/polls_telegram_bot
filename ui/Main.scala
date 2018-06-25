package ui

import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.Message

class Main extends TelegramBot with Polling {
  lazy val token: String = ""
  val commandExecutor = new CommandExecutor()

  override def receiveMessage(message: Message): Unit = {
    for (text <- message.text) {
      val result = commandExecutor.executeCommand(text, message.from)
      request(SendMessage(message.source, result))
    }
  }
}

package ExerciseSystemForSecurity

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import ExerciseSystemForSecurity.Messages._
import ExerciseSystemForSecurity.Scenario.Proto.ModeratorProto
import ExerciseSystemForSecurity.Scenario.First.ModeratorFirst


object Main extends App {
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, message) =>
    message match {
      case MessagesProto.StartModerator(main) => {
        val moderatorRef: ActorRef[Message] = context.spawn(ModeratorProto(), "moderator")
        moderatorRef ! MessagesProto.Start(main, moderatorRef)
        Behaviors.same
      }
      case MessagesProto.StopMain() => {
        println("Main関数を停止します")
        Behaviors.stopped
      }
      case MessagesFirst.StartModerator(main) => {
        val moderatorRef: ActorRef[Message] = context.spawn(ModeratorFirst(), "moderator")
        moderatorRef ! MessagesFirst.Start(main, moderatorRef)
        Behaviors.same
      }
      case MessagesFirst.StopMain() => {
        println("Main関数を停止します")
        Behaviors.stopped
      }
    }
  }

  val mainRef: ActorRef[Message] = ActorSystem(apply(), "main")
  //println("protoかfirstを選んでください")
  //val selectScenario = io.StdIn.readLine()
  //if (selectScenario == "proto") {
  //  mainRef ! MessagesProto.StartModerator(mainRef)
  //}
  //else if (selectScenario == "first") {
  //  mainRef ! MessagesFirst.StartModerator(mainRef)
  //}
  //else {
  //  println("不正な入力です。もう一度入力してください")
  //}
  mainRef ! MessagesFirst.StartModerator(mainRef)
}


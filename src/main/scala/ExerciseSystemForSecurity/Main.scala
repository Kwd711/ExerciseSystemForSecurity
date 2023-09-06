package ExerciseSystemForSecurity

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import ExerciseSystemForSecurity.Proto.ModeratorProto
import ExerciseSystemForSecurity.Messages._


object Main extends App {
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, message) =>
    message match {
      case MessagesProto.StartMain(main) => {
        val moderatorRef: ActorRef[Message] = context.spawn(ModeratorProto(), "moderator")
        moderatorRef ! MessagesProto.Start(main, moderatorRef)
        Behaviors.same
      }
      case MessagesProto.StopMain() => {
        context.log.info("Main関数を停止します")
        Behaviors.stopped
      }
    }
  }

  val mainRef: ActorRef[Message] = ActorSystem(apply(), "main")
  mainRef ! MessagesProto.StartMain(mainRef)
}


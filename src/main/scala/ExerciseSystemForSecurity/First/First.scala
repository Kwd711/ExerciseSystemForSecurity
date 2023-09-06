package ExerciseSystemForSecurity.First

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import ExerciseSystemForSecurity.Messages.Message
import ExerciseSystemForSecurity.Messages.MessagesFirst._
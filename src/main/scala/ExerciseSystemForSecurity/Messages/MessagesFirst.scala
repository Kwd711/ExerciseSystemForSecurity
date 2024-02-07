package ExerciseSystemForSecurity.Messages

import akka.actor.typed.ActorRef

object MessagesFirst{
  //Moderatorアクターを起動させるメッセージ
  final case class StartModerator(MainRef: ActorRef[Message]) extends Message

  //Moderatorアクターに最初に送るメッセージ
  final case class Start(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message]) extends Message

  //各アクターに初期情報を送るメッセージ
  final case class Init(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //moderatorアクターに次の段階へ進行させるメッセージ
  final case class NextStep(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //Userアクターにパケットを送信させる
  final case class SendPacket(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxyRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //パケット
  final case class Packet(from: String, Auth: String, Port: String, SuccessOrFailure: String, FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //Receptionアクターを入力待ち状態するメッセージ
  final case class WaitingInput(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxyRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //Firewallにフィルタリングルールを追加するメッセージ
  final case class AddRule(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //Proxyの認証機能を有効化するメッセージ
  final case class AuthOn(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //ProxyにACLを追加するメッセージ
  final case class AddACL(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //演習成功をMainアクターに知らせるメッセージ
  final case class SuccessExercise(MainRef: ActorRef[Message], ReceptionRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //時間切れで演習失敗をMainアクターに知らせるメッセージ
  //final case class FailedExercise(MainRef: ActorRef[Message], UserRef: ActorRef[Message], ProxywallRef: ActorRef[Message], FirewallRef: ActorRef[Message], ResultRef: ActorRef[Message]) extends Message

  //各アクターに停止を伝えるメッセージ
  final case class StopSystem() extends Message

  //Mainアクターを停止させるメッセージ
  final case class StopMain() extends Message
}
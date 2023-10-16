package ExerciseSystemForSecurity.Messages

import akka.actor.typed.ActorRef

object MessagesProto{
  //Mainアクターを起動させるメッセージ
  final case class StartMain(MainRef: ActorRef[Message]) extends Message
  //Mainアクターに最初に送るメッセージ
  final case class Start(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message]) extends Message
  //各アクターに初期情報を送るメッセージ
  final case class Init(MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], ReceptionRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //外部Webサーバにパケットを送信させるメッセージ
  final case class SendPacket(Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //通信パケットに見立てたメッセージ
  final case class Packet(From: String, To: String, Port: String, Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //C&Cサーバからマルウェアに指示を送る通信に見立てたメッセージ
  final case class Command(From: String, To: String, Port: String, MainRef: ActorRef[Message], ModeraotorRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //マルウェアがC&Cサーバへ送る窃取した情報に見立てたメッセージ
  final case class Reply(From: String, To: String, Port: String, MainRef: ActorRef[Message], ModeratorRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //ACLの追加
  final case class AddACL(PortNumber: String) extends Message
  //ACL一覧を出力
  final case class ShowACL() extends Message
  //通信の様子を可視化
  final case class ShowLog() extends Message
  //ルータの通信履歴を出力
  final case class ShowAll() extends Message
  //Receptionアクターを入力待ち状態するメッセージ
  final case class WaitingForInput(ReceptionRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], CCServerRef: ActorRef[Message]) extends Message
  //演習成功をMainアクターに知らせるメッセージ
  final case class SuccessExercise(MainRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //時間切れで演習失敗をMainアクターに知らせるメッセージ
  final case class FailedExercise(MainRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //各アクターに停止を伝えるメッセージ
  final case class StopSystem() extends Message
  //Mainアクターを停止させるメッセージ
  final case class StopMain() extends Message
}
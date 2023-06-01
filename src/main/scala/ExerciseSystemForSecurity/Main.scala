package ExerciseSystemForSecurity

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import ExerciseSystemForSecurity.Messages.{AddACL, Command, FailedExercise, Init, Message, Packet, Reply, SendPacket, ShowACL, ShowAll, ShowLog, Start, StopSystem, SuccessExercise}

import scala.util.Random

trait ExternalNetwork
trait InternalNetwork
trait DMZ

object Messages{
  trait Message
  //Mainアクターに最初に送るメッセージ
  final case class Start(MainRef: ActorRef[Message]) extends Message
  //各アクターに初期情報を送るメッセージ
  final case class Init(MainRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //外部Webサーバにパケットを送信させるメッセージ
  final case class SendPacket(Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //通信パケットに見立てたメッセージ
  final case class Packet(From: String, To: String, Port: String, Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //C&Cサーバからマルウェアに指示を送る通信に見立てたメッセージ
  final case class Command(From: String, To: String, Port: String, MainRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //マルウェアがC&Cサーバへ送る窃取した情報に見立てたメッセージ
  final case class Reply(From: String, To: String, Port: String, MainRef: ActorRef[Message], Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //ACLの追加
  final case class AddACL(PortNumber: String) extends Message
  //ACL一覧を出力
  final case class ShowACL() extends Message
  //通信の様子を可視化
  final case class ShowLog() extends Message
  //ルータの通信履歴を出力
  final case class ShowAll() extends Message
  //演習成功をMainアクターに知らせるメッセージ
  final case class SuccessExercise(Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //時間切れで演習失敗をMainアクターに知らせるメッセージ
  final case class FailedExercise(Client1Ref: ActorRef[Message], Client2Ref: ActorRef[Message], Client3Ref: ActorRef[Message], SwitchRef: ActorRef[Message], RouterRef: ActorRef[Message], FirewallRef: ActorRef[Message], ExternalWebServerRef: ActorRef[Message], CCServerRef: ActorRef[Message], MalwareRef: ActorRef[Message]) extends Message
  //各アクターに停止を伝えるメッセージ
  final case class StopSystem() extends Message
}



object ExternalWebServer extends ExternalNetwork {
  val name: String = "WebServer"
  def apply(): Behavior[Message] = Behaviors.receive {(context, message) =>
    message match{
      case Init(main, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware) => {
        //初期メッセージを受け取ったら自身にパケットを送信するようにメッセージを自己送信する
        externalwebserver ! SendPacket(client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case SendPacket(client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware) => {
        //宛先、ポート番号をランダムに選ぶ
        val NameList = List("Client1", "Client2", "Client3")
        val PortList = List("443")
        val chooseNameNumber = Random.nextInt(NameList.length)
        val choosePortNumber = Random.nextInt(PortList.length)
        //ファイアーウォールにパケットを送信する
        firewall ! Packet(name, NameList(chooseNameNumber), PortList(choosePortNumber), client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware)
        //10秒の待ち時間を挿入
        try Thread.sleep(10 * 1000)
        catch {
          case e: InterruptedException =>
            e.printStackTrace()
        }
        //自身に再びパケットを送信するようにメッセージを自己送信する
        externalwebserver ! SendPacket(client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}


object CCServer{
  val name: String = "C&CServer"
  var counter: Int = 0
  var ShowLog: Boolean = false
  val InfectedClientName: String = "Client3"
  val PortNumber = "xxx"
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Init(main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        //演習開始から1分後に攻撃開始
        try Thread.sleep(60 * 1 * 1000)
        catch {
          case e: InterruptedException =>
            e.printStackTrace()
        }
        ccserver ! Command(name, InfectedClientName, PortNumber, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        try Thread.sleep(10 * 1000)
        catch {
          case e: InterruptedException =>
            e.printStackTrace()
        }
        counter += 1
        firewall ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        if (counter == 30){
          main ! FailedExercise(client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
        else {
          if (ShowLog == true) {
            context.log.info(s"${to} receive packet From:${from} Port:${port}")
            ccserver ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
            Behaviors.same
          }
          else {
            ccserver ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
            Behaviors.same
          }
        }
      }
      case ShowLog() => {
        ShowLog = true
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Firewall{
  var AccessControlList = List[String]()
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Packet(from, to,  port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware) => {
        if (AccessControlList.isEmpty) {
          router ! Packet(from, to, port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware)
        }
        else if (AccessControlList.contains(port))  {
          router ! Packet(from, to, port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware)
        }
        else{
          context.log.info("許可されていないポートです")
        }
        Behaviors.same
      }
      case Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        if (AccessControlList.isEmpty) {
          router ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
        else {
          context.log.info("不正な通信を遮断しました")
          main ! SuccessExercise(client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
      }
      case Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        ccserver ! Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case AddACL(portnumber) => {
        AccessControlList = AccessControlList :+ portnumber
        context.log.info(s"ACLが追加されました。追加ポート番号${portnumber}")
        Behaviors.same
      }
      case ShowACL() => {
        println(AccessControlList)
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Router{
  var LogList = List[(String, String, String)]()
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Packet(from, to, port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware) => {
        val Log = (s"From:${from}",s"To:${to}", s"Port:${port}")
        LogList = LogList :+ Log
        switch ! Packet(from, to, port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        val Log = (s"From:${from}",s"To:${to}", s"Port:${port}")
        LogList = LogList :+ Log
        switch ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        val Log = (s"From:${from}", s"To:${to}", s"Port:${port}")
        LogList = LogList :+ Log
        firewall ! Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case ShowAll() => {
        var n = 0
        val len = LogList.length
        while (n < len) {
          println(LogList(n))
          n += 1
        }
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Switch{
  def apply(): Behavior[Message] = Behaviors.receive {(context, message) =>
    message match {
      case Packet(from, to, port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware) => {
        if (to == Client1.name) {
          client1 ! Packet(from, to, port, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
        else if (to == Client2.name) {
          client2 ! Packet(from, to, port, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
        else if (to == Client3.name) {
          client3 ! Packet(from, to, port, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
        else {
          context.log.info("宛先が不明です")
          Behaviors.same
        }
      }
      case Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
          client3 ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
      }
      case Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        router ! Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }

  }
}

object Client1 extends InternalNetwork {
  val name: String = "Client1"
  var ShowLog: Boolean = false
  def apply(): Behavior[Message] = Behaviors.receive {(context, message) =>
    message match {
      case Packet(from, to, port, client1, client2, client3, switch, router, firewall,externalwebserver, ccserver, malware) => {
        if(ShowLog == true) {
          context.log.info(s"${to} receive packet From:${from} Port:${port}")
          Behaviors.same
        }
        else {
          Behaviors.same
        }
      }
      case ShowLog() => {
        ShowLog = true
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Client2 extends InternalNetwork{
  val name: String = "Client2"
  var ShowLog: Boolean = false
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Packet(from, to, port, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        if (ShowLog == true) {
          context.log.info(s"${to} receive packet From:${from} Port:${port}")
          Behaviors.same
        }
        else {
          Behaviors.same
        }
      }
      case ShowLog() => {
        ShowLog = true
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Client3 extends InternalNetwork{
  val name: String = "Client3"
  var ShowLog: Boolean = false
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Packet(from, to, port, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        if (ShowLog == true) {
          context.log.info(s"${to} receive packet From:${from} Port:${port}")
          Behaviors.same
        }
        else {
          Behaviors.same
        }
      }
      case Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        if (ShowLog == true) {
          context.log.info(s"${to} receive packet From:${from} Port:${port}")
          malware ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
        else {
          malware ! Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
          Behaviors.same
        }
      }
      case Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        switch ! Reply(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case ShowLog() => {
        ShowLog = true
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Malware{
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Command(from, to, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        //10秒の待ち時間を挿入
        try Thread.sleep(10 * 1000)
        catch {
          case e: InterruptedException =>
            e.printStackTrace()
        }
        client3 ! Reply(to, from, port, main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware)
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Reception {
  def apply(): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Init(main, client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        val command = io.StdIn.readLine().split(' ')
        if (command(0) == "AddACL") {
          firewall ! AddACL(command(1))
        }
        else if (command(0) == "ShowACL") {
          firewall ! ShowACL()
        }
        else if (command(0) == "ShowLog") {
          ccserver ! ShowLog()
          client1 ! ShowLog()
          client2 ! ShowLog()
          client3 ! ShowLog()
        }
        else if (command(0) == "ShowAll") {
          router ! ShowAll()
        }
        else {
          context.log.info("コマンドが見つかりません")
        }
        receptionAgain(context, message)

        def receptionAgain(context: ActorContext[Message], message: Message): Behavior[Message] = {
          val command = io.StdIn.readLine().split(' ')
          if (command(0) == "AddACL") {
            firewall ! AddACL(command(1))
          }
          else if (command(0) == "ShowACL") {
            firewall ! ShowACL()
          }
          else if (command(0) == "ShowLog") {
            ccserver ! ShowLog()
            client1 ! ShowLog()
            client2 ! ShowLog()
            client3 ! ShowLog()
          }
          else if (command(0) == "ShowAll") {
            router ! ShowAll()
          }
          else {
            context.log.info("コマンドが見つかりません")
          }
          receptionAgain(context, message)
        }

        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Main extends App {
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, message) =>
    message match {
      case Start(main) => {
        val RecptionRef = context.spawn(Reception(), "Reception")
        val Client1Ref = context.spawn(Client1(), "Client1")
        val Client2Ref = context.spawn(Client2(), "Client2")
        val Client3Ref = context.spawn(Client3(), "Client3")
        val SwitchRef = context.spawn(Switch(), "Switch")
        val RouterRef = context.spawn(Router(), "Router")
        val FirewallRef = context.spawn(Firewall(), "Firewall")
        val ExternalWebServerRef = context.spawn(ExternalWebServer(), "ExternalWebServer")
        val CCServerRef = context.spawn(CCServer(), "CCServer")
        val MalwareRef = context.spawn(Malware(), "Malware")
        RecptionRef ! Init(main, Client1Ref, Client2Ref, Client3Ref, SwitchRef, RouterRef, FirewallRef, ExternalWebServerRef, CCServerRef, MalwareRef)
        ExternalWebServerRef ! Init(main, Client1Ref, Client2Ref, Client3Ref, SwitchRef, RouterRef, FirewallRef, ExternalWebServerRef, CCServerRef, MalwareRef)
        CCServerRef ! Init(main, Client1Ref, Client2Ref, Client3Ref, SwitchRef, RouterRef, FirewallRef, ExternalWebServerRef, CCServerRef, MalwareRef)
        Behaviors.same
      }
      case SuccessExercise(client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        client1 ! StopSystem()
        client2 ! StopSystem()
        client3 ! StopSystem()
        switch ! StopSystem()
        router ! StopSystem()
        firewall ! StopSystem()
        externalwebserver ! StopSystem()
        ccserver ! StopSystem()
        malware ! StopSystem()
        context.log.info("演習クリアです。おめでとうございます。")
        Behaviors.stopped
      }
      case FailedExercise(client1, client2, client3, switch, router, firewall, externalwebserver, ccserver, malware) => {
        client1 ! StopSystem()
        client2 ! StopSystem()
        client3 ! StopSystem()
        switch ! StopSystem()
        router ! StopSystem()
        firewall ! StopSystem()
        externalwebserver ! StopSystem()
        ccserver ! StopSystem()
        malware ! StopSystem()
        context.log.info("時間切れです。もう一度挑戦しましょう。")
        Behaviors.stopped
      }
    }
  }

  val mainRef: ActorRef[Message] = ActorSystem(apply, "main")
  mainRef ! Start(mainRef)
}


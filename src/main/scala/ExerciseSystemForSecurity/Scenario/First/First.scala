package ExerciseSystemForSecurity.Scenario.First

import ExerciseSystemForSecurity.Messages.Message
import ExerciseSystemForSecurity.Messages.MessagesFirst.{FailedExercise, Init, Start, StopMain, StopSystem, SuccessExercise, WaitingForInput, NextStep, SendPacket, Packet, AddRule, AuthOn, AddACL}
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.util.Random

object Result{
  def apply(): Behavior[Message] = Behaviors.receive[Message] {(context, messege) =>
    messege match {
      case Packet(from, auth, port, sof, firewall, result) => {
        println(s"(送信元:${from}, ${auth}, 使用ポート:${port}, ${sof})")
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Firewall {
  var filtering: Int = 0
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, messege) =>
    messege match {
      case Packet(from, auth, port, sof, firewall, result) => {
        filtering match {
          case 0 => {
            result ! Packet(from, auth, port, sof, firewall, result)
          }
          case 1 => {
            if (from == "Proxy") {
              result ! Packet(from, auth, port, sof, firewall, result)
            }
            else {
              result ! Packet(from, auth, port, "フィルタリングルールによりFirewallで遮断", firewall, result)
            }
          }
        }
        Behaviors.same
      }
      case AddRule(main, moderator, reception, user, proxy, firewall, result) => {
        filtering = 1
        println("\nフィルタリングルールが追加されました")
        moderator ! NextStep(main, moderator, reception, user, proxy, firewall, result)
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Proxy{
  var authfunc: Int= 0
  var acl: Int= 0
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, messege) =>
    messege match {
      case Packet(from, auth, port, sof, firewall, result) => {
        authfunc match {
          case 0 => {
            firewall ! Packet("Proxy", auth, port, sof, firewall, result)
          }
          case 1 => {
            if (auth == "認証情報あり") {
              acl match {
                case 0 => {
                  firewall ! Packet("Proxy", auth, port, sof, firewall, result)
                }
                case 1 => {
                  if (port == "80" || port == "443") {
                    firewall ! Packet("Proxy", auth, port, sof, firewall, result)
                  }
                  else {
                    firewall ! Packet("Proxy", auth, port, "プロキシサーバのACLで許可されていないため遮断", firewall, result)
                  }
                }
              }
            }
            else {
              firewall ! Packet("Proxy", auth, port, "プロキシサーバの認証機能によりプロキシサーバで遮断", firewall, result)
            }
          }
        }
        Behaviors.same
      }
      case AuthOn(main, moderator, reception, user, proxy, firewall, result) => {
        authfunc = 1
        println("\nプロキシサーバの認証機能が有効になりました")
        moderator ! NextStep(main, moderator, reception, user, proxy, firewall, result)
        Behaviors.same
      }
      case AddACL(main, moderator, reception, user, proxy, firewall, result) => {
        acl = 1
        println("\nプロキシサーバにアクセスコントロールリストが追加されました")
        moderator ! NextStep(main, moderator, reception, user, proxy, firewall, result)
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object User{
  var counter: Int = 0
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, message) =>
    message match {
      case SendPacket(main, moderator, reception, user, proxy, firewall, result) => {
        if (counter <= 19) {
          counter += 1
          val AddressList = List(proxy, firewall)
          val AuthList = List("認証情報なし", "認証情報あり")
          val PortList = List("80", "443", "xxx")
          val chooseAddressNumber = Random.nextInt(AddressList.length)
          val chooseAuthNumber = Random.nextInt(AuthList.length)
          val choosePortNumber = Random.nextInt(PortList.length)
          AddressList(chooseAddressNumber) ! Packet("User", AuthList(chooseAuthNumber), PortList(choosePortNumber), "通過", firewall, result)
          //0.5秒の待ち時間を挿入
          try Thread.sleep(500)
          catch {
            case e: InterruptedException =>
              e.printStackTrace()
          }
          user ! SendPacket(main, moderator, reception, user, proxy, firewall, result)
        }
        else {
          counter = 0
          moderator ! NextStep(main, moderator, reception, user, proxy, firewall, result)
        }
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object Reception {
  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, messege) =>
    messege match {
      case WaitingForInput(main, moderator, reception, user, proxy, firewall, result) => {
        val command = io.StdIn.readLine()
        command match {
          case "run" => {
            user ! SendPacket(main, moderator, reception, user, proxy, firewall, result)
          }
          case "filtering" => {
            firewall ! AddRule(main, moderator, reception, user, proxy, firewall, result)
          }
          case "authfunc" => {
            proxy ! AuthOn(main, moderator, reception, user, proxy, firewall, result)
          }
          case "addacl" => {
            proxy ! AddACL(main, moderator, reception, user, proxy, firewall, result)
          }
          case _ => {
            println("操作可能なコマンドではありません。もう一度入力してください")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
        }
        Behaviors.same
      }
      case StopSystem() => {
        Behaviors.stopped
      }
    }
  }
}

object ModeratorFirst{

  var step: Int = 0

  def apply(): Behavior[Message] = Behaviors.receive[Message] { (context, message) =>
    message match {
      case Start(main, moderator) => {
        val ReceptionRef = context.spawn(Reception(), "Reception")
        val UserRef = context.spawn(User(), "User")
        val ProxyRef = context.spawn(Proxy(), "Proxy")
        val FirewallRef = context.spawn(Firewall(), "Firewall")
        val ResultRef = context.spawn(Result(), "Result")

        moderator ! NextStep(main, moderator, ReceptionRef, UserRef, ProxyRef, FirewallRef, ResultRef)

        Behaviors.same
      }
      case NextStep(main, moderator, reception, user, proxy, firewall, result) => {
        step match {
          case 0 => {
            step = 1
            println("まずはrunコマンドを使って通信状況を確認してみましょう(ログを20個出力すると自動的に停止します)")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 1 => {
            step = 2
            println("\nプロキシサーバ経由せずに直接外部とやりとりしている通信が存在するようです。\nfilteringコマンドで送信元がプロキシサーバでない通信を遮断するフィルタリングルールを追加しましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 2 => {
            step = 3
            //1秒の待ち時間を挿入
            try Thread.sleep(1 * 1000)
            catch {
              case e: InterruptedException =>
                e.printStackTrace()
            }
            println("runコマンドで再び通信状況を確認してみましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 3 => {
            step = 4
            println("\nプロキシサーバを経由しない通信の遮断に成功しました。しかし、認証情報を持たない通信がプロキシサーバを通過しているようです。\nauthfuncコマンドでプロキシサーバの認証機能を有効化しましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 4 => {
            step = 5
            //1秒の待ち時間を挿入
            try Thread.sleep(1 * 1000)
            catch {
              case e: InterruptedException =>
                e.printStackTrace()
            }
            println("runコマンドで再び通信状況を確認してみましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 5 => {
            step = 6
            println("\n認証情報を持たない通信の遮断に成功しました。しかし、HTTP通信で用いられる80番ポートやHTTPS通信で用いられる443番ポート以外を使用する不審な通信があるようです。\naddaclコマンドで80番ポートと443番ポート以外を使用する通信を禁止するアクセスコントロールリスト(ACL)をプロキシサーバに追加しましょう。")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 6 => {
            step = 7
            //1秒の待ち時間を挿入
            try Thread.sleep(1 * 1000)
            catch {
              case e: InterruptedException =>
                e.printStackTrace()
            }
            println("runコマンドで再び通信状況を確認してみましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 7 => {
            moderator ! SuccessExercise(main, user, proxy, firewall, result)
          }
        }
        Behaviors.same
      }
      case SuccessExercise(main, user, proxy, firewall, result) => {
        user ! StopSystem()
        proxy ! StopSystem()
        firewall ! StopSystem()
        result ! StopSystem()
        println("\n演習クリアです。おめでとうございます。")
        main ! StopMain()
        Behaviors.stopped
      }
    }
  }
}
package ExerciseSystemForSecurity.Scenario.First

import ExerciseSystemForSecurity.Messages.Message
import ExerciseSystemForSecurity.Messages.MessagesFirst.{AddACL, AddRule, AuthOn, FailedExercise, Init, NextStep, Packet, SendPacket, Start, StopMain, StopSystem, SuccessExercise, WaitingForInput}
import ExerciseSystemForSecurity.Scenario.First.ModeratorFirst.{NetworkAA, step}
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
        io.StdIn.readLine() match {
          case "start" => {
            moderator ! NextStep(main, moderator, reception, user, proxy, firewall, result)
          }
          case "run" => {
            NetworkAA(step)
            user ! SendPacket(main, moderator, reception, user, proxy, firewall, result)
          }
          case "addrule" => {
            firewall ! AddRule(main, moderator, reception, user, proxy, firewall, result)
          }
          case "authfunc" => {
            proxy ! AuthOn(main, moderator, reception, user, proxy, firewall, result)
          }
          case "addacl" => {
            proxy ! AddACL(main, moderator, reception, user, proxy, firewall, result)
          }
          case null => {
            println("nullが入力されています")
          }
          case _ => {
            //println(command)
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
            println("近年脅威を増してきている高度標的型攻撃は、攻撃者が不特定多数を対象として自身の技術力を誇示することを目的に行う重箱の隅をつつくような従来のサイバー攻撃とは異なり、\n特定の企業や個人を対象として情報の改ざんや窃取を目的として行う攻撃手法です。")
            println("")
            println("高度標的型攻撃では")
            println("\n1.偵察\n2.武器化\n3.配送\n4.攻撃\n5.インストール\n6.遠隔操作\n7.目的実行")
            println("\nの7段階を経て攻撃が行われるとされています。")
            println("")
            println("このうち1~5段階目までは手口の変化が激しく、その全てに対応することが難しくなっています。")
            println("しかし、6段階目の遠隔操作の段階で行われる基盤構築や内部調査の手法はそれほど変化が大きくなく、対策の有効性が継続しやすい部分になります。")
            println("")
            println("そこで高度標的型攻撃への対応策としては上記７段階のうち1~5段階目にあたる攻撃者に侵入されることを防ぐことを目的とした「入口対策」以上に\n6段階目での侵入拡大を防ぐことを目的とした「内部対策」を充実させることが重要となっています。")
            println("")
            println("今回の演習ではその内部対策の一例としてファイアウォールやプロキシサーバの機能を用いてリモートコントロール通信経路の確立を防ぐ演習に取り組んでもらいます。")
            println("")
            println("それではstartコマンドを入力して演習を始めてください。")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 1 => {
            step += 1
            println("まずはrunコマンドを使って通信状況を確認してみましょう(ログを20個出力すると自動的に停止します)")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 2 => {
            step += 1
            println("\nプロキシサーバ経由せずに直接外部とやりとりしている通信が存在するようです。\nマルウェアの52%がプロキシを経由せずに攻撃者と直接通信しようとする傾向にあり、ユーザ端末が外部ネットワークと直接通信を行うこと制限するだけで約半数のマルウェアの通信を防ぐことができます。\naddruleコマンドで送信元がプロキシサーバでない通信を遮断するフィルタリングルールを追加しましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 3 => {
            step += 1
            //1秒の待ち時間を挿入
            try Thread.sleep(1 * 1000)
            catch {
              case e: InterruptedException =>
                e.printStackTrace()
            }
            println("runコマンドで再び通信状況を確認してみましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 4 => {
            step += 1
            println("\nプロキシサーバを経由しない通信の遮断に成功しました。しかし、認証情報を持たない通信がプロキシサーバを通過しているようです。\n27%のマルウェアは認証機能のあるプロキシに対応していません。\nauthfuncコマンドでプロキシサーバの認証機能を有効化し、認証機能に未対応のマルウェアの通信を防ぎましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 5 => {
            step += 1
            //1秒の待ち時間を挿入
            try Thread.sleep(1 * 1000)
            catch {
              case e: InterruptedException =>
                e.printStackTrace()
            }
            println("runコマンドで再び通信状況を確認してみましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 6 => {
            step += 1
            println("\n認証情報を持たない通信の遮断に成功しました。しかし、HTTP通信で用いられる80番ポートやHTTPS通信で用いられる443番ポート以外を使用する不審な通信があるようです。\naddaclコマンドで80番ポートと443番ポート以外を使用する通信を禁止するアクセスコントロールリスト(ACL)をプロキシサーバに追加しましょう。")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 7 => {
            step  += 1
            //1秒の待ち時間を挿入
            try Thread.sleep(1 * 1000)
            catch {
              case e: InterruptedException =>
                e.printStackTrace()
            }
            println("runコマンドで再び通信状況を確認してみましょう")
            reception ! WaitingForInput(main, moderator, reception, user, proxy, firewall, result)
          }
          case 8 => {
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

  def NetworkAA(step : Int) : Unit = {
    step match {
      case 2 => {
        println("<現在のネットワーク図>")
        println("                 Internet　　　　　　　　　     ")
        println("                     ↑　　　　　　　　　　　     ")
        println("                     |                       ")
        println("-------------------------------------------  ")
        println("|                 Firewall                 | ")
        println("|          フィルタリングルール:未設定          | ")
        println("-------------------------------------------")
        println("                    ↑　　　　　　　　　　↑　　　　 ")
        println("                    |                |       ")
        println("                    |                |       ")
        println("                    |                |       ")
        println("                    |                |       ")
        println("        －－－－－－－-                 |       ")
        println("        | －－－－－－－－－－－－－－－   |       ")
        println("        | |                      |   |       ")
        println("        | |                      |   |       ")
        println("        | ↓                      |   |  　　  ")
        println(" ----------------         --------------------")
        println(" |     Proxy     |        |                  |")
        println(" | 認証機能：無効   |        |       User       |")
        println(" | ACL:未設定     |        |                  |")
        println(" ----------------         --------------------")
      }
      case 4 => {
        println("<現在のネットワーク図>")
        println("                 Internet　　　　　　　　　                                            Internet　　　　　　　　　     ")
        println("                     ↑　　　　　　　　　　　                                                ↑　　　　　　　　　　　     ")
        println("                     |                                                                  |                       ")
        println("-------------------------------------------                        -------------------------------------------  ")
        println("|                 Firewall                 |                       |                 Firewall                 | ")
        println("|          フィルタリングルール:未設定          |                       | フィルタリングルール:送信元がProxyの通信のみ許可  | ")
        println("-------------------------------------------                        -------------------------------------------")
        println("                    ↑　　　　　　　　　　↑　　　　                                          ↑　　　　　　　　　　　　　　 ")
        println("                    |                |                                                |                       ")
        println("                    |                |                                                |                       ")
        println("                    |                |                      ＼                        |                       ")
        println("                    |                |       ----------------  ＼                     |                       ")
        println("        －－－－－－－                  |       ----------------  ／         －－－－－－－-                       ")
        println("        | －－－－－－－－－－－－－－－   |                       ／           | －－－－－－－－－－－－－－－          ")
        println("        | |                      |   |                                    | |                      |          ")
        println("        | |                      |   |                                    | |                      |          ")
        println("        | ↓                      |   |  　　                               | ↓                      |     　　  ")
        println(" ----------------         --------------------                      ----------------         --------------------")
        println(" |     Proxy     |        |                  |                      |     Proxy     |        |                  |")
        println(" | 認証機能：無効   |        |       User       |                      | 認証機能：無効  |         |       User       |")
        println(" | ACL:未設定     |        |                  |                      | ACL:未設定     |         |                  |")
        println(" ----------------         --------------------                      ----------------         --------------------")
      }
      case 6 => {
        println("<現在のネットワーク図>")
        println("                 Internet　　　　　　　　　                                            Internet　　　　　　　　　     ")
        println("                     ↑　　　　　　　　　　　                                                ↑　　　　　　　　　　　     ")
        println("                     |                                                                  |                       ")
        println("-------------------------------------------                        -------------------------------------------  ")
        println("|                 Firewall                 |                       |                 Firewall                 | ")
        println("| フィルタリングルール:送信元がProxyの通信のみ許可  |                      | フィルタリングルール:送信元がProxyの通信のみ許可  | ")
        println("-------------------------------------------                        -------------------------------------------")
        println("                    ↑　　　　　　　　　　 　　　　                                          ↑　　　　　　　　　　　　　　 ")
        println("                    |                                                                 |                       ")
        println("                    |                                                                 |                       ")
        println("                    |                                       ＼                        |                       ")
        println("                    |                        ----------------  ＼                     |                       ")
        println("        －－－－－－－                          ----------------  ／         －－－－－－－-                       ")
        println("        | －－－－－－－－－－－－－－－                           ／           | －－－－－－－－－－－－－－－          ")
        println("        | |                      |                                        | |                      |          ")
        println("        | |                      |                                        | |                      |          ")
        println("        | ↓                      |      　　                               | ↓                      |     　　  ")
        println(" ----------------         --------------------                      ----------------         --------------------")
        println(" |     Proxy     |        |                  |                      |     Proxy     |        |                  |")
        println(" | 認証機能：無効   |        |       User       |                      | 認証機能：有効  |         |       User       |")
        println(" | ACL:未設定     |        |                  |                      | ACL:未設定     |         |                  |")
        println(" ----------------         --------------------                      ----------------         --------------------")

      }
      case 8 => {
        println("<現在のネットワーク図>")
        println("                 Internet　　　　　　　　　                                            Internet　　　　　　　　　     ")
        println("                     ↑　　　　　　　　　　　                                                ↑　　　　　　　　　　　     ")
        println("                     |                                                                  |                       ")
        println("-------------------------------------------                        -------------------------------------------  ")
        println("|                 Firewall                 |                       |                 Firewall                 | ")
        println("| フィルタリングルール:送信元がProxyの通信のみ許可  |                      | フィルタリングルール:送信元がProxyの通信のみ許可  | ")
        println("-------------------------------------------                        -------------------------------------------")
        println("                    ↑　　　　　　　　　　 　　　　                                          ↑　　　　　　　　　　　　　　 ")
        println("                    |                                                                 |                       ")
        println("                    |                                                                 |                       ")
        println("                    |                                       ＼                        |                       ")
        println("                    |                        ----------------  ＼                     |                       ")
        println("        －－－－－－－                          ----------------  ／         －－－－－－－-                       ")
        println("        | －－－－－－－－－－－－－－－                           ／           | －－－－－－－－－－－－－－－          ")
        println("        | |                      |                                        | |                      |          ")
        println("        | |                      |                                        | |                      |          ")
        println("        | ↓                      |      　　                               | ↓                      |     　　  ")
        println(" ----------------         --------------------                      ----------------         --------------------")
        println(" |     Proxy     |        |                  |                      |     Proxy     |        |                  |")
        println(" | 認証機能：有効   |        |       User       |                      | 認証機能：有効  |         |       User       |")
        println(" | ACL:未設定     |        |                  |                      | ACL:80, 443   |         |                  |")
        println(" ----------------         --------------------                      ----------------         --------------------")

      }
    }
    }
  }
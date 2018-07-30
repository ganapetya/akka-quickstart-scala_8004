package com.peter.actors.game

import akka.actor.{Actor, ActorRef, ActorSystem, Props}


case object Start

case object Started

case object Go

case object ShellArrived

case object StopTank

case object Shoot

case object Strike

case object Advance

case object StartComputer


object GameStart extends App {

  val system: ActorSystem = ActorSystem("gameAkka")

  val field: ActorRef = system.actorOf(BattleFiled.props, "field")

  field ! Start

}

object BattleFiled {
  def props = Props(new BattleFiled)
}

class BattleFiled extends Actor {



  val maxTanks = 10

  def receive = {

    case Start => {
      println("Starting the game...")

      for (n <- 1 to maxTanks) {
        val tank = context.actorOf(Tank.props(), "tank" + n)
        tank ! Start
      }
      //sender ! Started
    }
  }
}

object Tank {
  def props() = Props(new Tank())
}

class Tank() extends Actor {

  /**
    *
    * Scala discourages using of vars.
    * But here they are in place , because this actor has a state
    *
    **/

  var distance = 0
  var step = 2
  var maxDistance = 500
  var shell = 0
  var arrivedShellsCounter = 0
  var computerInterrupt = false

  val computer = context.actorOf(ComputerFSM.props(self))
  computer ! StartComputer


  def receive = {

    case Start => {
      println("staring the tank")
      self ! Go
    }

    case Go => {

      distance = distance + step

      if (distance % 10 == 0) {
            println("Tank " + self + " shoots!")
            shell = shell + 1
            val shellActor = context.actorOf(Shell.props(distance), "shell-" + shell)
            shellActor ! Shoot
      }

      if ((distance < maxDistance) && !computerInterrupt)
        self ! Go
      else
        println("Tank " + self + " stops at distance " + distance)
    }

    case ShellArrived => {
      arrivedShellsCounter += 1
      println("Arrived shell number " + arrivedShellsCounter + " with id " + sender)
      computer ! Strike
    }

    case StopTank => {
      computerInterrupt = true
      println("Tank " + self + " interrupted by computer ")
    }

  }

}


object Shell {
  def props(distance: Int) = Props(new Shell(distance))
}

class Shell(var distance: Int) extends Actor {

  var step = 3

  var maxDistance = distance + 3000

  def receive = {

    case Shoot => {
      self ! Advance
    }
    case Advance => {

      distance = distance + step

      if (distance < maxDistance) {
        self ! Advance
      }
      else {
        println("Shell " + self + " arrived - distance " + distance + " !!!")
        context.parent ! ShellArrived
      }
    }
  }
}





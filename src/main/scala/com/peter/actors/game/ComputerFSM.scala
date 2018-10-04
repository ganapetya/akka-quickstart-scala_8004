package com.peter.actors.game


import akka.actor.{ActorRef, FSM, Props}


/**
  * States
  */

sealed trait State

case object Inactive extends State

case object OnDuty extends State

case object Hit extends State


/**
  * State data (Or "data, contained by the state")
  */

sealed trait Data

case object Uninitialized extends Data

class StrikesDataStorage(val startTime: Long) extends Data {

  private var shellsCounter = 0;

  def addShell = shellsCounter = shellsCounter + 1

  def getShellsCounter = shellsCounter

}

/**
  * Factory of computers
  */

object ComputerFSM{
  def props(tank:ActorRef) = Props(new ComputerFSM(tank))
}


/**
  * Computer itself
  *
  */
class ComputerFSM(tank: ActorRef) extends FSM[State, Data] {

  startWith(Inactive, Uninitialized)

  when(Inactive) {
    case Event(StartComputer, Uninitialized) => goto(OnDuty) using new StrikesDataStorage(System.currentTimeMillis)
    case Event(Strike, _) => stay
  }

  onTransition {
    case OnDuty -> Hit =>
      print("sends stop command to tank")
      tank ! StopTank
  }

  when(OnDuty) {
    case Event(Strike, stateDate) => {
      val storage = stateDate.asInstanceOf[StrikesDataStorage]
      storage.addShell
      val shells = storage.getShellsCounter
      if (shells >= 3) {
        print("HIT state begins now")
        goto(Hit) using storage
      } else {
        print("ON_DUTY state continues")
        stay using storage
      }
    }
  }

  import scala.concurrent.duration._

  when(Hit, stateTimeout = 5 second) {
    case _ => goto(Inactive)
  }

  initialize


}

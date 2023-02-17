package part_2_actors

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object ActorState {

  /* Exercise
  * use the setup method to create a word counter which
  * - splits each message into words
  * - keep track of the total number of words received so far
  * - for each message - log the current number of words plus the total */

  object WordCounter {
    def apply(): Behavior[String] = Behaviors.setup { context =>
      // the setup method is capable of managing actor logic and state

      var totalWords = 0
      def splitWords(msg: String) = msg.split(" ")

      Behaviors.receiveMessage { message =>
        val messageLengthInWords = splitWords(message).length
        totalWords += messageLengthInWords
        context.log.info(s"Message received: $message\nCurrent message Length: $messageLengthInWords : Total message Length: $totalWords")
        Behaviors.same
      }
    }
  }

  def demoWordCounter() = {
    val actorSystem = ActorSystem(EvolvedWordCounter(), "WordCounterSystem")

    actorSystem ! "There are many people"
    actorSystem ! "Who are you"
    actorSystem ! "My name is John Doe"

    Thread.sleep(1000)
    actorSystem.terminate()
  }

  trait SimpleThing
  case object EatChocolate extends SimpleThing
  case object CleanUpTheFloor extends SimpleThing
  case object LearnAkka extends SimpleThing

  /*
  * Message types must be immutable and serializable
  * - case classes or objects
  * - use flat type hierarchy
  * */

  object SimpleHuman {
    def apply(): Behavior[SimpleThing] = Behaviors.setup { context =>
      var happiness = 0

      Behaviors.receiveMessage {
        case EatChocolate =>
          context.log.info(s"[$happiness] Eating chocolate")
          happiness += 1
          Behaviors.same
        case CleanUpTheFloor =>
          context.log.info(s"[$happiness] Wiping the floor, ugh ...")
          happiness -= 2
          Behaviors.same
        case LearnAkka =>
          context.log.info(s"[$happiness] Learning akka, YAY!")
          happiness += 99
          Behaviors.same
      }
    }
  }

  def demoSimpleHuman: Unit = {
    val human = ActorSystem(EvolvedHuman(), "SimpleHumanSystem")

    human ! LearnAkka
    human ! EatChocolate
    (1 to 10).foreach(_ =>human ! CleanUpTheFloor)

    Thread.sleep(1000)
    human.terminate()
  }

  object EvolvedHuman {
    def apply(): Behavior[SimpleThing] = statelessHuman(0)
    def statelessHuman(happiness: Int): Behavior[SimpleThing] = Behaviors.receive { (context, message) =>
      message match {
        case EatChocolate =>
          context.log.info(s"[$happiness] Eating chocolate")
          statelessHuman(happiness + 1)
          // this is NOT the same as Recursion
          // the new behaviour obtained by stateless human will be used to handle the next message that the actor will receive at some point in the future
          // it may not be even the same thread working on the next message on the same actor
        case CleanUpTheFloor =>
          context.log.info(s"[$happiness] Wiping the floor, ugh ...")
          statelessHuman(happiness - 2)
        case LearnAkka =>
          context.log.info(s"[$happiness] Learning akka, YAY")
          statelessHuman(happiness + 99)
      }
    }
  }

  /*
  * Tips for making a stateful actor with variables to a stateless actor without variables
  * - each var/mutable becomes an immutable method argument
  * - each state change for instance becomes changing the behaviour by invoking the method with a different argument(s)
  * */

  /* Exercise
  * - refactor the "stateful" word counter into a "stateless" version */

  object EvolvedWordCounter {
    def apply(): Behavior[String] = wordCounter(0)

    def wordCounter(ctr: Int): Behavior[String] = Behaviors.receive { (context, message) =>
      val len = message.split(" ").length
      context.log.info(s"Message: $message\nCurrent message length: $len, Total length: ${ctr + len}")
      wordCounter(ctr + len)
    }
  }

  def main(args: Array[String]) = {
    demoWordCounter()
    demoSimpleHuman
  }
}

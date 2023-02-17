package part_2_actors

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object ActorsIntro {

  // part 1 : behavior
  // behavior - in terms of what the actor will do when it receives a message
  val simpleActorBehavior: Behavior[String] = Behaviors.receiveMessage{ (message: String) =>
    /* Behavior[String] behavior the actor will take upon receiving this message */
    // logic for handling the message
    println(s"[simple actor] I have received: $message")

    // new Behavior for the NEXT message
    Behaviors.same
  }

  def demoSimpleActor() = {
    // part 2 : instantiate
    val actorSystem = ActorSystem(SimpleActor_V2(), "FirstActorSystem")

    // part 3 : communicate
    actorSystem ! "I am learning akka" // asynchronously send a message

    // part 4: gracefully shut down the actor system
    Thread.sleep(1000)
    actorSystem.terminate()
  }

  // things to note around code structure
  /*
  * We don't keep behaviours as stand alone values but rather as the apply methods of objects which makes closer to the object oriented paradigm
  * */

  object SimpleActor {
    def apply(): Behavior[String] = Behaviors.receiveMessage{ (message: String) =>
      /* Behavior[String] behavior the actor will take upon receiving this message */
      // logic for handling the message
      println(s"[simple actor] I have received: $message")

      // new Behavior for the NEXT message
      Behaviors.same
    }
  }

  object SimpleActor_V2 {
    def apply(): Behavior[String] = Behaviors.receive { (context, message) =>
      // context is a data structure (ActorContext) with access to a variety of APIs
      // the context stays the same when the actor is instantiated
      // it is created alongside the actor is being passed around underlying akka runtime to the user so that we have access to it.
      // simple example of an API offered by context : logging

      context.log.info(s"[simple actor] I have received message: $message")
      Behaviors.same
    }
  }

  object SimpleActor_V3 {
    def apply(): Behavior[String] = Behaviors.setup { context =>
      // actor "private" data and methods, behaviors etc.
      // CODE



      // the first behavior that the actor will use for it's first message
      Behaviors.receiveMessage { message =>
        context.log.info(s"[simple actor] I have received message: $message")
        Behaviors.same
      }
    }
  }

  /*
  * Exercises
  * 1. Define two "person" actor behaviors, which receive Strings:
  * - "happy" which logs your message e.g. I've received ____, That's great!
  * - "sad" _____, That's sucks :(
  *
  * 2. Change the actor behavior
  * - the happy behaviour will turn to sad() if it receives "Akka is bad"
  * - the sad person will turn to happy() if it receive "Akka is awesome"
  *
  * 3. Inspect my code and try to make it better.
  * */

  // 1)
  object HappyPerson {
    def apply(): Behavior[String] = Behaviors.receive { (context, message) =>
      context.log.info(s"[happy actor] I've received $message, That's great")
      Behaviors.same
    }
  }

  object SadPerson {
    def apply(): Behavior[String] = Behaviors.receive { (context, message) =>
      context.log.info(s"[sad actor] $message, That sucks!")
      Behaviors.same
    }
  }

  // improvement - Wrap in a single object and call method
  object Person {
    def happy(): Behavior[String] = Behaviors.receive { (context, message) =>

      if (message.equals("Akka is bad")) {
        context.log.info(s"Received $message, switching emotion to sad :(")
        sad()
      } else {
        context.log.info(s"[happy actor] I've received $message, That's great")
        Behaviors.same
      }
    }

    def sad(): Behavior[String] = Behaviors.receive { (context, message) =>
      if (message.equals("Akka is awesome")) {
        context.log.info(s"Received $message, switching emotion to happy :)")
        happy()
      } else {
        context.log.info(s"[sad actor] $message, That sucks!")
        Behaviors.same
      }
    }
  }

  def demoHappySad() = {
//    val actorSystem1 = ActorSystem(HappyPerson(), "HappyActorSystem")
//    val actorSystem2 = ActorSystem(SadPerson(), "SadActorSystem")
//
//    actorSystem1 ! "Yay"
//    actorSystem2 ! "Bleh"
//
//    Thread.sleep(1000)
//    actorSystem1.terminate()
//    actorSystem2.terminate()

    val actorSystem = ActorSystem(Person.happy(), "PersonActorSystem")

    actorSystem ! "Yay"
    actorSystem ! "Akka is bad"
    actorSystem ! "Aww"
    actorSystem ! "Akka is awesome"
    actorSystem ! "Hola"

    Thread.sleep(1000)
    actorSystem.terminate()
  }

  object WeirdActor {
    // wants to receive messages of type Int and String
    def apply(): Behavior[Any] = Behaviors.receive { (context, message) =>
      message match {
        case number: Int =>
          context.log.info(s"I've received an Int: $number")
          Behaviors.same
        case str: String =>
          context.log.info(s"I've received a String: $str ")
          Behaviors.same
      }
    }
  }

  // [Any] is a problem - pattern match is not exhaustive
  // solution - add wrapper types and type hierarchy (case classes/objects)
  object BetterActor {
    trait Message
    case class IntMessage(number: Int) extends Message
    case class StringMessage(str: String) extends Message

    def apply(): Behavior[Any] = Behaviors.receive { (context, message) =>
      message match {
        case IntMessage(int) =>
          context.log.info(s"I've received an Int: $int")
          Behaviors.same
        case StringMessage(str) =>
          context.log.info(s"I've received a String: $str")
          Behaviors.same
      }
    }
  }

  def demoWeirdActor() = {
    import part_2_actors.ActorsIntro.BetterActor.{IntMessage, StringMessage}
    val weirdActorSysem = ActorSystem(BetterActor(), "WeirdActorSystem")
//    weirdActorSysem ! 42
//    weirdActorSysem ! "akka"
//    weirdActorSysem ! '\t'
    weirdActorSysem ! IntMessage(42)
    weirdActorSysem ! StringMessage("akka")
    // weirdActorSysem ! '\t'

    Thread.sleep(1000)
    weirdActorSysem.terminate()
  }

  def main(args: Array[String]): Unit = {
    demoSimpleActor()
    demoHappySad()
    demoWeirdActor()
  }
}

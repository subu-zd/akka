package part_2_actors

import akka.NotUsed
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object ChildActors {
  // actors can create other actors (child)
  /*
  * parent -> children -> grand children and so on.
  * actor hierarchy is like a tree like structure
  * root of the hierarchy  = "guardian" actor (created with an actor system)
  * ActorSystem created :
  * - the top level (root) guardian
  *   - system guardian (for akka internal messages)
  *   - user guardian (for our custom actors0
  *   - other auxiliary actors
  *
  * All OUR Actors are the child actors of the user guardian
  * */

  object Parent {
    trait Command
    case class CreateChild(name: String) extends Command
    case class TellChild(message: String) extends Command

    def apply(): Behavior[Command] = Behaviors.receive { (context, message) =>
      message match {
        case CreateChild(name) =>
          context.log.info(s"[parent] creating child with name: $name")
          // creating a child actor reference
          // this is used to send messages to this child

          val childRef: ActorRef[String] = context.spawn(Child(), name)
          active(childRef)
      }
    }

    def active(childRef: ActorRef[String]): Behavior[Command] = Behaviors.receive { (context, message) =>
      message match {
        case TellChild(message) =>
          context.log.info(s"[parent] sending message: $message, to child")
          childRef ! message // using an ActorRef we can send a message to another actor
          Behaviors.same
        case _ =>
          context.log.info("[parent] command not supported")
          Behaviors.same
      }
    }
  }

  object Child {
    def apply(): Behavior[String] = Behaviors.receive { (context, message) =>
      context.log.info(s"[child] received message: $message")
      Behaviors.same
    }
  }

  def demoParentChild() = {
    import Parent._

    val userGuardianBehavior: Behavior[NotUsed] = Behaviors.setup { context =>
      // set up all the important actors in your applications
      // set up the initial interaction between the actors

      val parent = context.spawn(Parent(), "parent")
      parent ! CreateChild("child")
      parent ! TellChild("hey kid, you there")

      // user guardian usually has no behavior of its own
      Behaviors.empty
    }

    val system = ActorSystem(userGuardianBehavior, "DemoParentChild")
    Thread.sleep(1000)
    system.terminate()
  }

  /* Exercise
  * - write a Parent_V2 for multiple children
  *  */

  object Parent_V2 {
    trait Command
    case class CreateChild(name: String) extends Command
    case class TellChild(name: String, message: String) extends Command

    def apply(): Behavior[Command] = ???
  }

  def main(args: Array[String]): Unit = {
    demoParentChild()
  }
}

/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import sbt.testing._
import org.scalajs.testinterface.TestUtils.{loadModule, newInstance}
import scala.language.reflectiveCalls
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

private abstract class ScalaCheckRunner(
  val args: Array[String],
  val remoteArgs: Array[String],
  val loader: ClassLoader
) extends Runner {

  type Prop = Gen[(String, Test.Result[String])]

  val successCount = new AtomicInteger(0)
  val failureCount = new AtomicInteger(0)
  val errorCount = new AtomicInteger(0)
  val testCount = new AtomicInteger(0)

  def deserializeTask(task: String, deserializer: String => TaskDef) = {
    val taskDef = deserializer(task)
    val countTestSelectors = taskDef.selectors.toSeq.count {
      case _:TestSelector => true
      case _ => false
    }
    if (countTestSelectors == 0) rootTask(taskDef)
    else checkPropTask(taskDef)
  }

  def serializeTask(task: Task, serializer: TaskDef => String) =
    serializer(task.taskDef)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = taskDefs.map(rootTask)

  abstract class BaseTask(override val taskDef: TaskDef) extends Task {
    val tags: Array[String] = Array()

    val props: Seq[Prop] = {
      val fp = taskDef.fingerprint.asInstanceOf[SubclassFingerprint]
      val obj = if (fp.isModule) loadModule(taskDef.fullyQualifiedName,loader)
                else newInstance(taskDef.fullyQualifiedName, loader)(Seq())
      (obj.asInstanceOf[Properties]).properties
    }

    def execute(handler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit
    ): Unit  = continuation(execute(handler,loggers))
  }

  def rootTask(td: TaskDef) = new BaseTask(td) {
    def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
      props.zipWithIndex.toArray map { case (prop, idx) =>
        checkPropTask(new TaskDef(td.fullyQualifiedName, td.fingerprint,
          td.explicitlySpecified, Array(new TestSelector(idx.toString)))
        )
      }
  }

  def checkPropTask(taskDef: TaskDef) = new BaseTask(taskDef) {
    val idxs = taskDef.selectors flatMap {
      case ts: TestSelector => Array(ts.testName.toInt)
      case _ => Array.empty[Int]
    }

    def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
      idxs flatMap { idx =>
        @tailrec def evalProp(p: Prop, prms: Gen.Parameters): (String, Test.Result[String]) = {
          val r = p.apply(prms)
          val size = 0 // TODO: Calculate next size
          r.next match {
            case Some(p1) => evalProp(p1, prms.withSize(size).withSeed(r.nextSeed))
            case None => r.value.get
          }
        }

        val prms = Gen.Parameters.default.withSize(0)
        val (name, result) = evalProp(props(idx), prms)

        val event = new Event {
          val status = result match {
            case _:Test.Passed[String] => Status.Success
            case _:Test.Proved[String] => Status.Success
            case _:Test.Failed[String] => Status.Failure
            case _:Test.Exhausted[String] => Status.Failure
            case _:Test.PropException[String] => Status.Error
          }
          val throwable = result match {
            case Test.PropException(_, e) => new OptionalThrowable(e)
            case _:Test.Failed[String] => new OptionalThrowable(
              new Exception(result.toString)
            )
            case _ => new OptionalThrowable()
          }
          val fullyQualifiedName = taskDef.fullyQualifiedName
          val selector = new TestSelector(name)
          val fingerprint = taskDef.fingerprint
          val duration = -1L
        }

        handler.handle(event)

        event.status match {
          case Status.Success => successCount.incrementAndGet()
          case Status.Error => errorCount.incrementAndGet()
          case Status.Skipped => errorCount.incrementAndGet()
          case Status.Failure => failureCount.incrementAndGet()
          case _ => failureCount.incrementAndGet()
        }
        testCount.incrementAndGet()

        val s = if (event.status == Status.Success) "+" else "!"
        val n = if (name.isEmpty) taskDef.fullyQualifiedName else name
        val logMsg = s"$s $n: $result"
        loggers.foreach(l => l.info(logMsg))

        Array.empty[Task]
      }
  }

}

final class ScalaCheckFramework extends Framework {

  private def mkFP(mod: Boolean, cname: String, noArgCons: Boolean = true) =
    new SubclassFingerprint {
      def superclassName(): String = cname
      val isModule = mod
      def requireNoArgConstructor(): Boolean = noArgCons
    }

  val name = "ScalaCheck"

  def fingerprints: Array[Fingerprint] = Array(
    mkFP(false, "org.scalacheck.Properties"),
    mkFP(true, "org.scalacheck.Properties")
  )

  def runner(args: Array[String], remoteArgs: Array[String],
    loader: ClassLoader
  ): Runner = new ScalaCheckRunner(args, remoteArgs, loader) {

    def receiveMessage(msg: String): Option[String] = msg(0) match {
      case 'd' =>
        val Array(t,s,f,e) = msg.tail.split(',')
        testCount.addAndGet(t.toInt)
        successCount.addAndGet(s.toInt)
        failureCount.addAndGet(f.toInt)
        errorCount.addAndGet(e.toInt)
        None
    }

    def done = {
      val heading = if (testCount.get == successCount.get) "Passed" else "Failed"
      s"$heading: Total $testCount, Failed $failureCount, Errors $errorCount, Passed $successCount"
    }

  }

  def slaveRunner(args: Array[String], remoteArgs: Array[String],
    loader: ClassLoader, send: String => Unit
  ): Runner = new ScalaCheckRunner(args, remoteArgs, loader) {

    def receiveMessage(msg: String) = None

    def done = {
      send(s"d$testCount,$successCount,$failureCount,$errorCount")
      ""
    }

  }

}

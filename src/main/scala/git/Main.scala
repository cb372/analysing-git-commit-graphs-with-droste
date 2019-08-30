package git

import java.io.File
import java.time.Instant

import org.eclipse.jgit._
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.revwalk._
import org.eclipse.jgit.storage.file._
import org.eclipse.jgit.diff._
import org.eclipse.jgit.util.io.DisabledOutputStream

import cats.instances.int._
import cats.instances.map._
import cats.instances.tuple._
import cats.syntax.show._
import cats.{Show, Monoid}

import higherkindness.droste._
import higherkindness.droste.data.Fix
import higherkindness.droste.data.list._

import scala.collection.JavaConverters._

object Main extends App {

  /*
   * Helper function to interact with jgit and clean up afterwards
   */
  def walkCommits[A](gitRepo: Repository)(f: RevWalk => A): A = {
    val head = gitRepo.resolve("HEAD")
    val walk = new RevWalk(gitRepo)
    walk.markStart(walk.parseCommit(head))
    try {
      f(walk)
    } finally {
      walk.dispose()
    }
  }

  val fromRevWalkCoalgebra: Coalgebra[ListF[RevCommit, ?], RevWalk] = Coalgebra { walk =>
    val nextCommit = walk.next()
    if (nextCommit == null)
      NilF
    else
      ConsF(nextCommit, walk)
  }

  def countAlgebra[A]: Algebra[ListF[A, ?], Int] = Algebra {
    case NilF => 0
    case ConsF(_, count) => count + 1
  }

  implicit val showCommit: Show[RevCommit] = commit =>
    s"${commit.getId.abbreviate(8).name} - ${Instant.ofEpochSecond(commit.getCommitTime)} - ${commit.getAuthorIdent.getName} - ${commit.getShortMessage}"

  def showAlgebra[A: Show]: Algebra[ListF[A, ?], String] = Algebra {
    case NilF              => ""
    case ConsF(head, tail) => s"${head.show}\n$tail"
  }

  def fromRevWalkRCoalgebra(days: Int): RCoalgebra[List[RevCommit], ListF[RevCommit, ?], RevWalk] = RCoalgebra { walk =>
    val nextCommit = walk.next()
    if (nextCommit == null)
      NilF
    else {
      if (Instant.ofEpochSecond(nextCommit.getCommitTime).plusSeconds(60 * 60 * 24 * days).isAfter(Instant.now())) {
        ConsF(nextCommit, Right(walk))
      } else {
        ConsF(nextCommit, Left(Nil))
      }
    }
  }

  val commitsPerContributorAlgebra: Algebra[ListF[RevCommit, ?], Map[String, Int]] = Algebra {
    case NilF =>
      Map.empty[String, Int]
    case ConsF(commit, commitsPerContributor) =>
      Monoid[Map[String, Int]].combine(Map(commit.getCommitterIdent.getName -> 1), commitsPerContributor)
  }

  def pairsAlgebra[A]: RAlgebra[List[A], ListF[A, ?], List[(A, A)]] = RAlgebra {
    case NilF               => Nil
    case ConsF(x, (Nil, r)) => Nil
    case ConsF(x, (y :: xs, r)) => (x, y) :: r
  }

  def listCommits(gitRepo: Repository): List[RevCommit] =
    walkCommits(gitRepo)(scheme.ana(fromRevWalkCoalgebra))

  def countCommits(gitRepo: Repository): Int =
    walkCommits(gitRepo)(scheme.hylo(countAlgebra[RevCommit], fromRevWalkCoalgebra))

  def showCommits(gitRepo: Repository): String =
    walkCommits(gitRepo)(scheme.hylo(showAlgebra[RevCommit], fromRevWalkCoalgebra))

  def commitsPerContributor(gitRepo: Repository): Map[String, Int] =
    walkCommits(gitRepo)(scheme.hylo(commitsPerContributorAlgebra, fromRevWalkCoalgebra))

  /*
   * TODO: Ideally I'd like to do commits per contributor another way,
   * by composing 2 smaller algebras into a larger one, but I couldn't
   * find a way to do what I wanted. Maybe I'm barking up the wrong tree.
   * The idea was something like:
   *
   * 1. ana(fromRevWalkCoalgebra) to get a Fix[ListF[RevCommit, ?]]
   * 2. process that result to extract the contributor name from each commit,
   *      so now we have a Fix[ListF[Map[String, Int], ?]]
   * 3. cata on that using a generic `monoidAlgebra` to combine all the maps into one.
   */

  /*
   * Note: this actually returns all the commits in the last N days, plus one extra commit.
   * This is because of the way apomorphisms work: you stop the recursion one step too late.
   * It would be more accurate, but less fun, to use a plain old Coalgebra that stops iterating
   * and returns NilF at the right time.
   */
  def listCommitsInLastNDays(gitRepo: Repository, days: Int): List[RevCommit] =
    walkCommits(gitRepo)(scheme.zoo.apo[ListF[RevCommit, ?], RevWalk, List[RevCommit]](fromRevWalkRCoalgebra(days)))

  def pairsOfCommits(gitRepo: Repository): List[(RevCommit, RevCommit)] =
    walkCommits(gitRepo) { walk =>
      val commits: List[RevCommit] = scheme.ana(fromRevWalkCoalgebra).apply(walk)
      scheme.zoo.para(pairsAlgebra[RevCommit]).apply(commits)
    }

  case class ShortStat(insertions: Int, deletions: Int)

  implicit val shortStatMonoid: Monoid[ShortStat] = new Monoid[ShortStat] {
    def empty: ShortStat = ShortStat(0, 0)
    def combine(a: ShortStat, b: ShortStat): ShortStat =
      ShortStat(a.insertions + b.insertions, a.deletions + b.deletions)
  }

  implicit val shortStatShow: Show[ShortStat] =
    ss => s"${ss.insertions} insertions, ${ss.deletions} deletions"

  def shortStat(gitRepo: Repository, newCommit: RevCommit, oldCommit: RevCommit): ShortStat = {
    val formatter = new DiffFormatter(DisabledOutputStream.INSTANCE)
    try {
      formatter.setRepository(gitRepo)
      val diffEntries = formatter.scan(oldCommit.getTree, newCommit.getTree).asScala
      val shortStats =
        for {
          fileHeader <- diffEntries.map(formatter.toFileHeader)
          edit <- fileHeader.toEditList.asScala
        } yield toShortStat(edit)
      Monoid[ShortStat].combineAll(shortStats.toList)
    } finally {
      formatter.close()
    }
  }

  def toShortStat(edit: Edit): ShortStat = {
    // Note: not entirely sure about any of this...
    if (edit.getType == Edit.Type.INSERT)
      ShortStat(insertions = edit.getLengthB, deletions = 0)
    else if (edit.getType == Edit.Type.DELETE)
      ShortStat(insertions = 0, deletions = edit.getLengthA)
    else if (edit.getType == Edit.Type.REPLACE)
      ShortStat(insertions = edit.getLengthB, deletions = edit.getLengthA)
    else
      Monoid[ShortStat].empty
  }

  def calculateShortStats(gitRepo: Repository): List[ShortStat] = {
    pairsOfCommits(gitRepo).map {
      case (newCommit, oldCommit) => shortStat(gitRepo, newCommit, oldCommit)
    }
  }

  val gitRepo = new FileRepositoryBuilder().setWorkTree(new File("../droste")).build()

  println(listCommits(gitRepo))
  println("===")
  println(countCommits(gitRepo))
  println("===")
  println(showCommits(gitRepo))
  println("===")
  println(listCommitsInLastNDays(gitRepo, 120))
  println("===")
  println(commitsPerContributor(gitRepo))
  println("===")
  println(pairsOfCommits(gitRepo).take(10).map(_.show))
  println("===")
  println(calculateShortStats(gitRepo).take(10).map(_.show).mkString("\n"))
  println("===")

}

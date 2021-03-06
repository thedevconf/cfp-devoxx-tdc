/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 Association du Paris Java User Group.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package models

import library.{Stats, Redis}
import org.joda.time.{Instant, DateTime}
import scala.math.BigDecimal.RoundingMode

/**
 * When a CFP admin checks or perform a review on a talk, we store this event.
 *
 * We use a SET to store which proposal was reviewed
 * We use a ZSET to store the score
 *
 * Author: nicolas martignole
 * Created: 11/11/2013 10:21
 */
case class Review(reviewer: String, proposalId: String, vote: Int, date: DateTime)

object Review {

  def conferenceId = ConferenceDescriptor.current().eventCode

  // We use 4 different Redis objects
  // 1) a SET to keep an history of all proposals we voted for
  // 2) a SET to keep an history of all voters for a proposal
  // 3) a Sorted Set where key is "reviewer email" and value is the vote. It keeps only the latest vote.
  //     If you vote more than once for a talk, it keeps only the latest vote
  // 4) a HASH with the Review as JSON. We keep an history of updates on a proposal
  def voteForProposal(proposalId: String, reviewerUUID: String, vote: Int) = Redis.pool.withClient {
    implicit client =>
      val tx = client.multi()
      tx.sadd(s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewerUUID", proposalId)
      tx.sadd(s"Proposals:$conferenceId:Reviewed:ByProposal:$proposalId", reviewerUUID)
      tx.zadd(s"Proposals:$conferenceId:Votes:$proposalId", vote, reviewerUUID) // if the vote does already exist, Redis updates the existing vote. reviewer is a discriminator on Redis.
      tx.zadd(s"Proposals:$conferenceId:Dates:$proposalId", new Instant().getMillis, reviewerUUID + "__" + vote) // Store when this user voted for this talk
      tx.exec()
      Event.storeEvent(Event(proposalId, reviewerUUID, s"Voted $vote"))
  }

  /**
    * Allows a reviewer to remove his vote on a proposal
    *
    * @param proposalId proposal that will have a vote removed
    * @param reviewerUUID reviewer that is removing his vote
    * @return
    */
  def removeVoteForProposal(proposalId: String, reviewerUUID: String) = {
      removeVote(proposalId, reviewerUUID)
      Event.storeEvent(Event(proposalId, reviewerUUID, "Removed its vote on this talk"))
  }

  /**
    * Allows an admin to remove a vote from a proposal
    *
    * @param adminUUID admin that is removing the vote
    * @param proposalId proposal that will have a vote removed
    * @param reviewerUUID reviewer that will have his vote for the proposal removed
    * @return
    */
  def removeVoteForProposal(adminUUID:String, proposalId: String, reviewerUUID: String) = {
      removeVote(proposalId, reviewerUUID)
      Event.storeEvent(Event(proposalId, adminUUID, "Admin removed a vote on this talk"))
  }

  /**
    * removes the vote from the Redis database
    *
    * @param proposalId proposal that will have a vote removed
    * @param reviewerUUID reviewer that will have his vote for the proposal removed
    * @return
    */
  private def removeVote(proposalId: String, reviewerUUID: String) = Redis.pool.withClient {
    implicit client =>
      val tx = client.multi()
      tx.srem(s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewerUUID", proposalId)
      tx.srem(s"Proposals:$conferenceId:Reviewed:ByProposal:$proposalId", reviewerUUID)
      tx.zrem(s"Proposals:$conferenceId:Votes:$proposalId", reviewerUUID) // if the vote does already exist, Redis updates the existing vote. reviewer is a discriminator on Redis.
      tx.zrem(s"Proposals:$conferenceId:Dates:$proposalId", reviewerUUID + "__DEL")
      tx.exec()
  }

  def archiveAllVotesOnProposal(proposalId: String) = Redis.pool.withClient {
    implicit client =>
      val tx = client.multi()
      allVotesFor(proposalId).map {
        case (reviewer, _) =>
          tx.srem(s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewer", proposalId)
      }
      tx.del(s"Proposals:$conferenceId:Reviewed:ByProposal:$proposalId")
      tx.del(s"Proposals:$conferenceId:Votes:$proposalId") // if the vote does already exist, Redis updates the existing vote. reviewer is a discriminator on Redis.
      tx.del(s"Proposals:$conferenceId:Dates:$proposalId")
      tx.hdel(s"Computed:$conferenceId:Scores", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.hdel(s"Computed:$conferenceId:Voters", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.hdel(s"Computed:$conferenceId:Votes:ScoreAndCount", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.hdel(s"Computed:$conferenceId:Average", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.hdel(s"Computed:$conferenceId:VotersAbstention", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.hdel(s"Computed:$conferenceId:StandardDeviation", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.hdel(s"Computed:$conferenceId:Median", s"Proposals:$conferenceId:Votes:$proposalId")
      tx.exec()
      Review.computeAndGenerateVotes()
  }

  /*
  * Returns all the proposals for review if the user is admin,
  * otherwise returns only the proposals on the tracks the user is trackleader
  */
  def allProposalsNotReviewed(reviewerUUID: String): List[Proposal] = Redis.pool.withClient {
    implicit client =>
      // Take all SUBMITTED, remove approved and refused, then removed the ones already reviewed
      val allProposalIDsForReview = client.sdiff(s"Proposals:$conferenceId:ByState:${ProposalState.SUBMITTED.code}", s"ApprovedById:$conferenceId",
        s"RefusedById:$conferenceId", s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewerUUID")
      val allProposalsForReview = Proposal.loadProposalByIDs(allProposalIDsForReview, ProposalState.SUBMITTED)
      if(Webuser.hasAccessToAdmin(reviewerUUID)) {
        allProposalsForReview
      } else {
        allProposalsForReview.filter(p => TrackLeader.isTrackLeader(p.track.id, reviewerUUID))
      }
  }

  def deleteVoteForProposal(proposalId: String) = Redis.pool.withClient {
    implicit client =>
      play.Logger.of("proposal.Review").debug(s"Deleting vote for proposal $proposalId")
      val allAuthors = client.smembers(s"Proposals:$conferenceId:Reviewed:ByProposal:$proposalId")
      val tx = client.multi()
      allAuthors.foreach {
        reviewerUUID: String =>
          tx.srem(s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewerUUID", proposalId)
      }
      tx.del(s"Proposals:$conferenceId:Reviewed:ByProposal:$proposalId")
      tx.del(s"Proposals:$conferenceId:Votes:$proposalId")
      tx.del(s"Proposals:$conferenceId:Dates:$proposalId")
      tx.exec()
  }

  val ReviewerAndVote = "(\\w+)__(\\d+)".r

  // Returns the history of votes for a proposal. If a reviewer changed its vote, we will also see it.
  def allHistoryOfVotes(proposalId: String): List[Review] = Redis.pool.withClient {
    implicit client =>
      // for instance ZREVRANGEBYSCORE Proposals:Dates:BQX-255 +inf -inf WITHSCORES
      //      1) "b14651a3cd78ab4fd03d522ebef81cdac1d5755c__2" //b14651a3.. = user uuid and 2 is the vote
      //      2) "1387058296080"                               // time stamp when the person voted
      //      3) "0867c4e2182ef84e2dfcd412e33e01a9bc98dac2__8"
      //      4) "1386781873312"

      val listOfReviewsAndVotes = client.zrevrangeByScoreWithScores(s"Proposals:$conferenceId:Dates:$proposalId", "+inf", "-inf")
      val history: List[Review] = listOfReviewsAndVotes.flatMap {
        tuple =>
          val reviewerAndVote = tuple._1
          val date = tuple._2
          reviewerAndVote match {
            // Regexp extractor
            case ReviewerAndVote(reviewer, vote) => Option(Review(reviewer, proposalId, vote.toInt, new Instant(date.toLong).toDateTime))
            case _ => None
          }
      }
      history
  }

  def currentScore(proposalId: String): Int = Redis.pool.withClient {
    client =>
      val allScores = client.zrevrangeByScoreWithScores(s"Proposals:$conferenceId:Votes:$proposalId", 10, 0).toList
      allScores.foldRight(0)((scoreAndReview, accumulated: Int) => accumulated + scoreAndReview._2.toInt)
  }

  def totalVoteFor(proposalId: String): Long = Redis.pool.withClient {
    client =>
      client.zcount(s"Proposals:$conferenceId:Votes:$proposalId", 0, 10) // how many votes between 0 and 10 ?
  }

  // If we remove those who voted "0" for a talk, how many votes do we have?
  def totalVoteCastFor(proposalId: String): Long = Redis.pool.withClient {
    implicit client =>
      client.zcount(s"Proposals:$conferenceId:Votes:$proposalId", 1, 10)
  }

  def averageScore(proposalId:String):Double = Redis.pool.withClient{
    client=>
      val allScores = client.zrangeByScoreWithScores(s"Proposals:$conferenceId:Votes:$proposalId", 1, 10).map(_._2)
      Stats.average(allScores)
  }

  type ReviewerAndVote = (String, Double)

  def allVotesFor(proposalId: String): List[ReviewerAndVote] = Redis.pool.withClient {
    implicit client =>
      client.zrevrangeByScoreWithScores(s"Proposals:$conferenceId:Votes:$proposalId", 10, 0).toList
  }

  type VotesPerProposal = (String, Long)

  def allProposalsAndReviews: List[VotesPerProposal] = Redis.pool.withClient {
    implicit client =>
      val onlyValidProposalIDs = Proposal.allProposalIDsNotDeleted
      val totalPerProposal = onlyValidProposalIDs.map {
        proposalId =>
          (proposalId, totalVoteCastFor(proposalId))
      }
      totalPerProposal.toList
  }

  def allProposalsWithNoVotes: Map[String, Proposal] = {
    val proposalIDs = allProposalsAndReviews.filter(_._2 == 0).map(_._1).toSet
    Proposal.loadAndParseProposals(proposalIDs)
  }

  def countAll(): Long = {
    val totalPerProposal = allProposalsAndReviews
    totalPerProposal.map(_._2).sum // total reviewed
  }

  def countWithNoVotes(): Long = {
    val totalPerProposal = allProposalsAndReviews.filter(_._2 == 0)
    totalPerProposal.size
  }

  def countWithVotes(): Long = {
    val totalPerProposal = allProposalsAndReviews.filterNot(_._2 == 0)
    totalPerProposal.size
  }

  def mostReviewed(): Option[VotesPerProposal] = {
    val maybeBestProposal = allProposalsAndReviews.sortBy(_._2).reverse.headOption
    maybeBestProposal
  }

  def bestReviewer(): Option[(String, Int)] = {
    totalReviewedByCFPuser().sortBy(_._2).reverse.headOption
  }

  // Worst reviewer is one that did review at least one talk.
  // We don't want to return those who did not review any talk yet
  def worstReviewer(): Option[(String, Int)] = {
    totalReviewedByCFPuser().sortBy(_._2).filterNot(_._2 == 0).headOption
  }

  def totalReviewedByCFPuser(): List[(String, Int)] = Redis.pool.withClient {
    implicit client =>
      Webuser.allCFPWebusers().map {
        webuser: Webuser =>
          val uuid = webuser.uuid
          val total = client.sdiff(s"Proposals:$conferenceId:Reviewed:ByAuthor:$uuid", s"Proposals:$conferenceId:ByState:" + ProposalState.DELETED.code, s"Proposals:$conferenceId:ByState:" + ProposalState.ARCHIVED.code, s"Proposals:$conferenceId:ByState:" + ProposalState.DRAFT.code, s"Proposals:$conferenceId:ByState:" + ProposalState.DECLINED.code)
          (uuid, total.size)
      }
  }

  def lastVoteByUserForOneProposal(reviewerUUID: String, proposalId: String): Option[Review] = Redis.pool.withClient {
    implicit client =>
      // If I voted for this proposal - O(1) very fast access
      if (client.sismember(s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewerUUID", proposalId)) {
        // Then ok, load the vote... O(log(N)+M) with N: nb of votes and M the number returned (all...)
        // this method use Redis zrevrangeByScoreWithScores so the list is already sorted
        // from the most recent vote to the oldest vote for a proposal.
        // The first Review with author = reviewerUUID is then the most recent vote for this talk
        allHistoryOfVotes(proposalId).find(review => review.reviewer == reviewerUUID)
      } else {
        None
      }
  }

  def allVotesFromUser(reviewerUUID: String): Set[(String, Option[Double])] = Redis.pool.withClient {
    implicit client =>
      client.smembers(s"Proposals:$conferenceId:Reviewed:ByAuthor:$reviewerUUID").flatMap {
        proposalId: String =>
          val score = Option(client.zscore(s"Proposals:$conferenceId:Votes:$proposalId", reviewerUUID))
          score match {
            case None =>
              val state = Proposal.findProposalState(proposalId)
              state.flatMap {
                case ProposalState.DRAFT => None
                case ProposalState.DECLINED => None
                case ProposalState.DELETED => None
                case ProposalState.REJECTED => None
                case ProposalState.ARCHIVED => None
                case ProposalState.UNKNOWN => None
                case other => Option((proposalId, None))
              }
            case Some(_) =>
              val state = Proposal.findProposalState(proposalId)
              state.flatMap {
                case ProposalState.DRAFT => None
                case ProposalState.DECLINED => None
                case ProposalState.DELETED => None
                case ProposalState.REJECTED => None
                case ProposalState.ARCHIVED => None
                case ProposalState.UNKNOWN => None
                case other =>
                  Option((proposalId, score.map(_.toDouble)))
              }
          }
      }
  }

  class Score(val s:Double) extends AnyVal
  class TotalVoter(val i:Int) extends AnyVal
  class TotalAbst(val i:Int) extends AnyVal
  class AverageNote(val n:Double) extends AnyVal
  class StandardDev(val d:Double) extends AnyVal


  /**
    * Returns allVotes but discard deleted/archived/draft proposals
    */
  def allVotes(): Map[String, (Score, TotalVoter, TotalAbst, AverageNote, StandardDev)] = Redis.pool.withClient {
    client =>
      val allVoters = client.hgetAll(s"Computed:$conferenceId:Voters")
      val allAbstentions = client.hgetAll(s"Computed:$conferenceId:VotersAbstention")
      val allAverages = client.hgetAll(s"Computed:$conferenceId:Average")
      val allStandardDev = client.hgetAll(s"Computed:$conferenceId:StandardDeviation")

      val allProposalIDSToRemove = Proposal.allProposalIDsDeletedArchivedOrDraft()

      client.hgetAll(s"Computed:$conferenceId:Scores").map {
        case (proposalKey: String, scores: String) if !allProposalIDSToRemove.contains(proposalKey)=>
          val proposalId = proposalKey.substring(proposalKey.lastIndexOf(":") + 1)
          (proposalId,
            (new Score(scores.toDouble),
              new TotalVoter(allVoters.get(proposalKey).map(_.toInt).getOrElse(0)),
              new TotalAbst(allAbstentions.get(proposalKey).map(_.toInt).getOrElse(0)),
              new AverageNote(allAverages.get(proposalKey).filterNot(_ == "nan").filterNot(_ == "-nan").map(d => BigDecimal(d.toDouble).setScale(3, RoundingMode.HALF_EVEN).toDouble).getOrElse(0.toDouble)),
              allStandardDev.get(proposalKey).filterNot(_ == "nan").filterNot(_ == "-nan").map {
                d =>
                  new StandardDev(BigDecimal(d.toDouble).setScale(3, RoundingMode.HALF_EVEN).toDouble)
              }.getOrElse(new StandardDev(0.toDouble))
            )
          )
      }
  }

  // internal function that upload to Redis a LUA Script
  // The function returns the Script SHA1
  val loadLUAScript: String = Redis.pool.withClient {
    client =>
      val script =
        s"""
          |local proposals = redis.call("KEYS", "Proposals:$conferenceId:Votes:*")
          |redis.call("DEL", "Computed:$conferenceId:Reviewer:Total")
          |redis.call("DEL", "Computed:$conferenceId:Reviewer:ReviewedOne")
          |redis.call("DEL", "Computed:$conferenceId:Scores")
          |redis.call("DEL", "Computed:$conferenceId:Voters")
          |redis.call("DEL", "Computed:$conferenceId:Average")
          |redis.call("DEL", "Computed:$conferenceId:Votes:ScoreAndCount")
          |redis.call("DEL", "Computed:$conferenceId:StandardDeviation")
          |redis.call("DEL", "Computed:$conferenceId:VotersAbstention")
          |redis.call("DEL", "Computed:$conferenceId:Median")
          |
          |for i = 1, #proposals do
          |  redis.log(redis.LOG_DEBUG, "----------------- " .. proposals[i])
          |
          |  redis.call("HSET", "Computed:$conferenceId:Scores", proposals[i], 0)
          |  redis.call("HSET", "Computed:$conferenceId:Voters", proposals[i], 0)
          |  redis.call("HSET", "Computed:$conferenceId:Average", proposals[i], 0)
          |  redis.call("HDEL", "Computed:$conferenceId:Votes:ScoreAndCount", proposals[i])
          |  redis.call("HDEL", "Computed:$conferenceId:VotersAbstention", proposals[i])
          |  redis.call("HDEL", "Computed:$conferenceId:StandardDeviation" , proposals[i])
          |
          |  local uuidAndScores = redis.call("ZRANGEBYSCORE", proposals[i], 1, 11, "WITHSCORES")
          |
          |  for j=1,#uuidAndScores,2 do
          |    redis.log(redis.LOG_DEBUG, "uuid:" ..  uuidAndScores[j] .. " score:" .. uuidAndScores[j + 1])
          |    redis.call("HINCRBY", "Computed:$conferenceId:Scores", proposals[i], uuidAndScores[j + 1])
          |    redis.call("HINCRBY", "Computed:$conferenceId:Voters", proposals[i], 1)
          |    redis.call("HINCRBY", "Computed:$conferenceId:Reviewer:Total", uuidAndScores[j], uuidAndScores[j + 1])
          |    redis.call("SADD", "Computed:$conferenceId:Reviewer:ReviewedOne",  uuidAndScores[j])
          |  end
          |
          |redis.call("HDEL", "Computed:$conferenceId:Median", proposals[i])
          |
          | local count = redis.call("HGET", "Computed:$conferenceId:Voters", proposals[i])
          | local total = redis.call("HGET", "Computed:$conferenceId:Scores", proposals[i])
          | local avg = 0
          |   if (count and total) then
          |        avg = tonumber(total)/tonumber(count)
          |        redis.call("HSET", "Computed:$conferenceId:Average", proposals[i], avg)
          |   end
          |
          | redis.log(redis.LOG_DEBUG, "Average: " .. avg)
          |
          |  local vm = 0
          |  local sum2 = 0
          |  local count2 = 0
          |  local standardDev
          |
          |  for z=1,#uuidAndScores,2 do
          |      vm = uuidAndScores[z + 1] - avg
          |      sum2 = sum2 + (vm * vm)
          |      count2 = count2 + 1
          |  end
          |
          | redis.log(redis.LOG_DEBUG, "Standard Deviation sum2: " .. sum2)
          | redis.log(redis.LOG_DEBUG, "Standard Deviation count2: " .. count2)
          | if  sum2 < 1  then
          |  standardDev = 0
          | else
          |  if(count2>1) then
          |     standardDev = math.sqrt(sum2 / (count2-1))
          |  else
          |    standardDev = 0
          |  end
          | end
          |
          |  redis.log(redis.LOG_DEBUG, "Standard Deviation: " .. standardDev)
          |  redis.call("HSET", "Computed:$conferenceId:StandardDeviation" , proposals[i], standardDev)
          |
          | local countAbstention = redis.call("ZCOUNT", proposals[i], 0, 0)
          | if(countAbstention>0) then
          |    redis.call("HSET", "Computed:$conferenceId:VotersAbstention" , proposals[i], countAbstention)
          | end
          |end
          |return #proposals
        """.stripMargin

      val sha1script = client.scriptLoad(script)
      play.Logger.of("models.Review").info("Uploaded LUA script to Redis " + sha1script)
      sha1script
  }

  def computeAndGenerateVotes() = Redis.pool.withClient {
    implicit client =>
      if (client.scriptExists(loadLUAScript)) {
        client.evalsha(loadLUAScript, 0)
      } else {
        play.Logger.of("models.Review").error("There is no LUA script to compute scores and votes on Redis")
      }
  }

  def allReviewersAndStats(): List[(String, Int, Int)] = Redis.pool.withClient {
    client =>
      // Remove reviewer that are not any longer part of CFP
      val validReviewers=client.smembers("Webuser:cfp")

      val allVoted = client.hgetAll(s"Computed:$conferenceId:Reviewer:Total").filter(uuidAndPoints => validReviewers.contains(uuidAndPoints._1)).map {
        case ( uuid: String,totalPoints: String) =>
          val nbrOfTalksReviewed = client.sdiff(s"Proposals:$conferenceId:Reviewed:ByAuthor:$uuid",
            s"Proposals:$conferenceId:ByState:" + ProposalState.DELETED.code,
            s"Proposals:$conferenceId:ByState:" + ProposalState.ARCHIVED.code,
            s"Proposals:$conferenceId:ByState:" + ProposalState.DRAFT.code).size
          (uuid, totalPoints.toInt, nbrOfTalksReviewed)
      }

      val noReviews = client.sdiff("Webuser:cfp", s"Computed:$conferenceId:Reviewer:ReviewedOne")
      val noReviewsAndNote = noReviews.map(uuid =>
        (uuid, 0, 0)
      )
      allVoted.toList ++ noReviewsAndNote.toList


  }

  def diffReviewBetween(firstUUID: String, secondUUID: String): Set[String] = Redis.pool.withClient {
    client =>
      client.sdiff(s"Proposals:$conferenceId:Reviewed:ByAuthor:$firstUUID",
        s"Proposals:$conferenceId:Reviewed:ByAuthor:$secondUUID",
        s"Proposals:$conferenceId:ByState:" + ProposalState.DELETED.code,
        s"Proposals:$conferenceId:ByState:" + ProposalState.ARCHIVED.code,
        s"Proposals:$conferenceId:ByState:" + ProposalState.DRAFT.code)
  }

}

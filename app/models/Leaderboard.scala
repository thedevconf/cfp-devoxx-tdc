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

import library.Redis
import play.api.libs.json.{Json,JsValue}

case class LeaderboardProposalStates (submitted:Int
     ,approved:Int
     ,accepted:Int
     ,declined:Int
     ,rejected:Int
     ,backup:Int
     ,total:Int)

object LeaderboardProposalStates {
  implicit val leaderboardProposalStatesFormat = Json.format[LeaderboardProposalStates]
}

/**
 * Leaderboard for stats, used by ZapActor.
 * Created by nicolas on 21/01/2014.
 */
object Leaderboard {

  def conferenceId = ConferenceDescriptor.current().eventCode
  
  def computeStats() = Redis.pool.withClient {
    implicit client =>

      // First, stats on Proposal, cause Computed:Reviewer:ReviewedOne is used for
      // the lazy ones
      Review.computeAndGenerateVotes()

      val tx = client.multi()

      //Total of Speakers
      val totalSpeakers = Speaker.countAll()
      tx.set(s"Leaderboard:$conferenceId:totalSpeakers", totalSpeakers.toString())

      //Total Approved Speakers
      val allApprovedIDs= ApprovedProposal.allApprovedSpeakerIDs()
      val totalApprovedSpeakers = allApprovedIDs.size
      tx.set(s"Leaderboard:$conferenceId:totalApprovedSpeakers", totalApprovedSpeakers.toString)

      //Total Refused Speakers
      val allRejectedIDs= ApprovedProposal.allRefusedSpeakerIDs()
      val refusedSpeakers = allRejectedIDs.diff(allApprovedIDs)
      val totalRefusedSpeakers = refusedSpeakers.size
      tx.set(s"Leaderboard:$conferenceId:totalRefusedSpeakers", totalRefusedSpeakers.toString)

      //Total Proposals
      val totalProposals = Proposal.countAll()
      tx.set(s"Leaderboard:$conferenceId:totalProposals", totalProposals.toString())

      //Total Proposals with Votes
      val totalWithVotes = Review.countWithVotes()
      tx.set(s"Leaderboard:$conferenceId:totalWithVotes", totalWithVotes.toString())

      //Total Proposals with No Votes
      val totalNoVotes = Review.countWithNoVotes()
      tx.set(s"Leaderboard:$conferenceId:totalNoVotes", totalNoVotes.toString())

      //Total Reviews
      val totalReviews = Review.countAll()
      tx.set(s"Leaderboard:$conferenceId:totalVotes", totalReviews.toString())

      // Proposal most reviewed
      val mostReviewed = Review.mostReviewed()
      mostReviewed.map{
        mr=>
        tx.set(s"Leaderboard:$conferenceId:mostReviewed:proposal", mr._1)
        tx.set(s"Leaderboard:$conferenceId:mostReviewed:score", mr._2.toString)
      }.getOrElse{
        tx.del(s"Leaderboard:$conferenceId:mostReviewed:proposal")
        tx.del(s"Leaderboard:$conferenceId:mostReviewed:score")
      }

      // Proposals by state
      val totalByState = Proposal.allActiveProposals().groupBy(_.state.code).map(pair => (pair._1,pair._2.size))
      tx.del(s"Leaderboard:$conferenceId:totalByState")
      totalByState.foreach {
        case (state: String, total: Int) =>
          tx.hset(s"Leaderboard:$conferenceId:totalByState", state, total.toString)
      }

      //Proposals submitted by track
      val totalSubmittedByTrack = Proposal.totalSubmittedByTrack()
      tx.del(s"Leaderboard:$conferenceId:totalSubmittedByTrack")
      totalSubmittedByTrack.map {
        case (track: Track, total: Int) =>
          tx.hset(s"Leaderboard:$conferenceId:totalSubmittedByTrack", track.id, total.toString)
      }

      //Proposals approved by track
      val allApproved = ApprovedProposal.allApproved()
      val totalApprovedByTrack =  allApproved.groupBy(_.track.label).map(trackAndProposals=>(trackAndProposals._1,trackAndProposals._2.size))
      tx.del(s"Leaderboard:$conferenceId:totalApprovedByTrack")
      totalApprovedByTrack.foreach {
        case (track: String, total: Int) =>
          tx.hset(s"Leaderboard:$conferenceId:totalApprovedByTrack", track, total.toString)
      }

      //Proposals accepted by track
      val totalAcceptedByTrack = Proposal.totalAcceptedByTrack()
      tx.del(s"Leaderboard:$conferenceId:totalAcceptedByTrack")
      totalAcceptedByTrack.map {
        case (track: Track, total: Int) =>
          tx.hset(s"Leaderboard:$conferenceId:totalAcceptedByTrack", track.label, total.toString)
      }

      //Proposals State by track
      import LeaderboardProposalStates.leaderboardProposalStatesFormat
      val allProposalStatesByTrack = computeProposalStatesByTrack()  
      tx.del(s"Leaderboard:$conferenceId:proposalStatesByTrack")
      allProposalStatesByTrack.map {
        case (track, stats) =>
          tx.hset(s"Leaderboard:$conferenceId:proposalStatesByTrack", track, Json.toJson(stats).toString)
      }

     tx.exec()
  }

  def totalSpeakers():Long = {
    getFromRedis(s"Leaderboard:$conferenceId:totalSpeakers")
  }

  def totalProposals():Long = {
    getFromRedis(s"Leaderboard:$conferenceId:totalProposals")
  }

  def totalVotes():Long = {
    getFromRedis(s"Leaderboard:$conferenceId:totalVotes")
  }

  def totalWithVotes():Long = {
    getFromRedis(s"Leaderboard:$conferenceId:totalWithVotes")
  }

  def totalNoVotes():Long = {
    getFromRedis(s"Leaderboard:$conferenceId:totalNoVotes")
  }

  def mostReviewed():Option[(String,String)] = {
    Redis.pool.withClient {
      implicit client =>
        for (proposalId <- client.get(s"Leaderboard:$conferenceId:mostReviewed:proposal");
             score <- client.get(s"Leaderboard:$conferenceId:mostReviewed:score")) yield (proposalId, score)
    }
  }

  def bestReviewer():Option[(String,String)] = Redis.pool.withClient {
    implicit client =>
      for (uuid <- client.get(s"Leaderboard:$conferenceId:bestReviewer:uuid");
           score <- client.get(s"Leaderboard:$conferenceId:bestReviewer:score")) yield (uuid, score)
  }

  // Returns the Reviewer that did at least one review, but the fewest reviews.
  def worstReviewer():Option[(String,String)] = Redis.pool.withClient {
    implicit client =>
      for (uuid <- client.get(s"Leaderboard:$conferenceId:worstReviewer:uuid");
           score <- client.get(s"Leaderboard:$conferenceId:worstReviewer:score")) yield (uuid, score)
  }

  // Returns the user that has the lowest reviewed number of proposals and the full list of cfp user that did not
  // yet reviewed any talk
  def lazyOnes():Map[String, String] = Redis.pool.withClient {
    implicit client =>
     val lazyOneWithOneVote = worstReviewer()
      // Take CFP members, remove admin and remove all webuser that reviewed at least one
     val otherThatHaveNoVotes  =  client.sdiff("Webuser:cfp", "Webuser:admin", s"Computed:$conferenceId:Reviewer:ReviewedOne" ).map(s=>(s,"0"))
     val toReturn = (lazyOneWithOneVote.toSet ++ otherThatHaveNoVotes).toMap
    toReturn
  }

  def totalSubmittedByTrack():Map[String,Int] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"Leaderboard:$conferenceId:totalSubmittedByTrack").map {
        case (key: String, value: String) =>
          (key, value.toInt)
      }
  }

  def totalSubmittedByType():Map[String,Int] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"Leaderboard:$conferenceId:totalSubmittedByType").map {
        case (key: String, value: String) =>
          (key, value.toInt)
      }
  }

  def totalAcceptedByTrack():Map[String,Int] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"Leaderboard:$conferenceId:totalAcceptedByTrack").map {
        case (key: String, value: String) =>
          (key, value.toInt)
      }
  }

  def totalAcceptedByType():Map[String,Int] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"Leaderboard:$conferenceId:totalAcceptedByType").map {
        case (key: String, value: String) =>
          (key, value.toInt)
      }
  }

  def totalApprovedByTrack(): Map[String,Int] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"Leaderboard:$conferenceId:totalApprovedByTrack").map {
        case (key: String, value: String) =>
          (key, value.toInt)
      }
  }

  def totalByState():List[(String,Int)] = Redis.pool.withClient {
    implicit client =>
      client.hgetAll(s"Leaderboard:$conferenceId:totalByState").map {
        case (key: String, value: String) =>
          (key, value.toInt)
      }.toList
  }

  private def getFromRedis(key: String): Long = Redis.pool.withClient {
    implicit client =>
      client.get(key).map(_.toLong).getOrElse(0L)
  }

  def totalApprovedSpeakers():Long ={
      getFromRedis(s"Leaderboard:$conferenceId:totalApprovedSpeakers")
  }

  def totalWithTickets():Long ={
      getFromRedis(s"Leaderboard:$conferenceId:totalWithTickets")
  }

  def totalRefusedSpeakers():Long={
    getFromRedis(s"Leaderboard:$conferenceId:totalRefusedSpeakers")
  }
  
  /*
   * returns a map with the Track as a key and a tuple with the sum of proposals by state in the folloging order
   * (submitted,approved,accepted,declined,rejected,backup)
   */
  private def computeProposalStatesByTrack(): Map[String,LeaderboardProposalStates] = {
    val allProposalsByTrack:Map[Track,List[Proposal]] = Proposal.allActiveProposals().groupBy(_.track)
    val allTracks = ConferenceDescriptor.ConferenceTracks.ALL.map( track => {
      if(allProposalsByTrack.isDefinedAt(track)) {
        val currentTrackProposals = allProposalsByTrack(track)
        val totalProposalsByState = currentTrackProposals.groupBy(_.state)
        (track.id,
          LeaderboardProposalStates(totalProposalsByState.applyOrElse(ProposalState.SUBMITTED,returnEmptyList).size,
            totalProposalsByState.applyOrElse(ProposalState.APPROVED,returnEmptyList).size,
            totalProposalsByState.applyOrElse(ProposalState.ACCEPTED,returnEmptyList).size,
            totalProposalsByState.applyOrElse(ProposalState.DECLINED,returnEmptyList).size,
            totalProposalsByState.applyOrElse(ProposalState.REJECTED,returnEmptyList).size,
            totalProposalsByState.applyOrElse(ProposalState.BACKUP,returnEmptyList).size,
            currentTrackProposals.size))
      } else {
        (track.id,LeaderboardProposalStates(0,0,0,0,0,0,0))
      }
    })
    allTracks.toMap
  }

  private def returnEmptyList(param:Any) = Nil
  
  /*
   * reads the last computed value for the proposal states by track
   */
  def allProposalStatesByTrack(): Map[String,LeaderboardProposalStates] = Redis.pool.withClient {
    implicit client =>
      import LeaderboardProposalStates.leaderboardProposalStatesFormat
      client.hgetAll(s"Leaderboard:$conferenceId:proposalStatesByTrack").map {
        case (track, stats) =>
          (track, Json.parse(stats).as[LeaderboardProposalStates])
      }
  }
}
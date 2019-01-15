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
import models.ConferenceDescriptor.{ConferenceProposalConfigurations, ConferenceProposalTypes}
import collection.JavaConverters._

/**
 * Approve or reject a proposal
 * Created by Nicolas Martignole on 29/01/2014.
 */
object ApprovedProposal {

  val conferenceId = ConferenceDescriptor.current().eventCode	

  def elasticSearchIndex():String={
    "proposals_"+ConferenceDescriptor.current().confUrlCode
  }

  val getTotal: Map[String, Int] = Map(
    ("conf.label", ConferenceProposalConfigurations.CONF.slotsCount)
    , ("uni.label", ConferenceProposalConfigurations.UNI.slotsCount)
    , ("tia.label", ConferenceProposalConfigurations.TIA.slotsCount)
    , ("lab.label", ConferenceProposalConfigurations.LAB.slotsCount)
    , ("quick.label", ConferenceProposalConfigurations.QUICK.slotsCount)
    , ("bof.label", ConferenceProposalConfigurations.BOF.slotsCount)
    , ("key.label", ConferenceProposalConfigurations.KEY.slotsCount)
    , ("ignite.label", ConferenceProposalConfigurations.IGNITE.slotsCount)
    , ("other.label", ConferenceProposalConfigurations.OTHER.slotsCount)
  )

  def countApproved(talkType: String): Long = Redis.pool.withClient {
    client =>
      talkType match {
        case null => 0
        case "all" =>
          ConferenceDescriptor.ConferenceProposalTypes.ALL.foldLeft(0L)((total,confType) => total + client.scard(s"Approved:${conferenceId}"+confType.id))
        case other =>
          client.scard(s"Approved:${conferenceId}:$talkType")
      }
  }

  def countRefused(talkType: String): Long = Redis.pool.withClient {
    client =>
      talkType match {
        case null => 0
        case "all" =>
          ConferenceDescriptor.ConferenceProposalTypes.ALL.foldLeft(0L)((total,confType) => total + client.scard(s"Refused:${conferenceId}"+confType.id))
        case other =>
          client.scard(s"Refused:${conferenceId}:$talkType")
      }
  }

  def reflectProposalChanges(proposal: Proposal) = Redis.pool.withClient {
    implicit client =>
      changeTalkType(proposal.id, proposal.talkType.id)
      recomputeAcceptedSpeakers()
  }

  def recomputeAcceptedSpeakers() = Redis.pool.withClient {
    implicit client =>
      val allSpeakerIDs = client.keys(s"ApprovedSpeakers:${conferenceId}:*")

      val tx = client.multi()
      allSpeakerIDs.foreach {
        speakerId =>
          tx.del(s"$speakerId")
      }
      allApproved().map {
        proposal =>
          tx.sadd(s"ApprovedSpeakers:${conferenceId}:" + proposal.mainSpeaker, proposal.id.toString)
          proposal.secondarySpeaker.map(secondarySpeaker => tx.sadd(s"ApprovedSpeakers:${conferenceId}:" + secondarySpeaker, proposal.id.toString))
          proposal.otherSpeakers.foreach {
            otherSpeaker: String =>
              tx.sadd(s"ApprovedSpeakers:${conferenceId}:" + otherSpeaker, proposal.id.toString)
          }
      }
      tx.exec()

  }

  // Update Approved or Refused total by conference type
  def changeTalkType(proposalId: String, newTalkType: String) = Redis.pool.withClient {
    client =>
      ConferenceDescriptor.ConferenceProposalTypes.ALL.foreach {
        proposalType =>
          if (client.sismember(s"Approved:${conferenceId}:${proposalType.id}", proposalId)) {
            val tx = client.multi()
            tx.srem(s"Approved:${conferenceId}:${proposalType.id}", proposalId)
            tx.sadd(s"Approved:${conferenceId}:$newTalkType", proposalId)
            tx.exec()
          }
          if (client.sismember(s"Refused:${conferenceId}:${proposalType.id}", proposalId)) {
            val tx = client.multi()
            tx.srem(s"Refused:${conferenceId}:${proposalType.id}", proposalId)
            tx.sadd(s"Refused:${conferenceId}:$newTalkType", proposalId)
            tx.exec()
          }
      }
  }

  def isApproved(proposal: Proposal): Boolean = {
    isApproved(proposal.id, proposal.talkType.id)
  }

  def isApproved(proposalId: String, talkType: String): Boolean = Redis.pool.withClient {
    client =>
      client.sismember(s"Approved:${conferenceId}:$talkType", proposalId)
  }

  // This is only for Attic controller, to fix an old bug on data (bug #159)
  // The bug was that a conference is approved, but then the speaker changes the
  // format to quickie, then the Approved:conf collection is not updated correctly
  def _loadApprovedCategoriesForTalk(proposal: Proposal): List[String] = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter { pc =>
      isApproved(proposal.id, pc.id)
    }.map(_.id)
  }

  def isRefused(proposal: Proposal): Boolean = {
    isRefused(proposal.id, proposal.talkType.id)
  }

  def isRefused(proposalId: String, talkType: String): Boolean = Redis.pool.withClient {
    client =>
      client.sismember(s"Refused:${conferenceId}:$talkType", proposalId)
  }

  def remainingSlots(talkType: String): Long = {
    var propType = ProposalConfiguration.parse(talkType)
    if (propType == ProposalConfiguration.UNKNOWN) {
      ProposalConfiguration.totalSlotsCount - countApproved("all")
    } else {
      propType.slotsCount - countApproved(talkType)
    }
  }

  def approve(proposal: Proposal) = Redis.pool.withClient {
    implicit client =>
      val tx = client.multi()
      tx.sadd(s"ApprovedById:${conferenceId}:", proposal.id.toString)
      tx.sadd(s"Approved:${conferenceId}:" + proposal.talkType.id, proposal.id.toString)
      tx.sadd(s"ApprovedSpeakers:${conferenceId}:" + proposal.mainSpeaker, proposal.id.toString)
      proposal.secondarySpeaker.map(secondarySpeaker => tx.sadd(s"ApprovedSpeakers:${conferenceId}:" + secondarySpeaker, proposal.id.toString))
      proposal.otherSpeakers.foreach {
        otherSpeaker: String =>
          tx.sadd(s"ApprovedSpeakers:${conferenceId}:" + otherSpeaker, proposal.id.toString)
      }
      tx.exec()
  }

  def refuse(proposal: Proposal) = Redis.pool.withClient {
    implicit client =>
      cancelApprove(proposal)
      val tx = client.multi()
      tx.sadd(s"RefusedById:${conferenceId}:", proposal.id.toString)
      tx.sadd(s"Refused:${conferenceId}:" + proposal.talkType.id, proposal.id.toString)

      tx.sadd(s"RefusedSpeakers:${conferenceId}:" + proposal.mainSpeaker, proposal.id.toString)
      proposal.secondarySpeaker.map(secondarySpeaker => tx.sadd(s"RefusedSpeakers:${conferenceId}:" + secondarySpeaker, proposal.id.toString))
      proposal.otherSpeakers.foreach {
        otherSpeaker: String =>
          tx.sadd(s"RefusedSpeakers:${conferenceId}:" + otherSpeaker, proposal.id.toString)
      }
      tx.exec()
  }

  def cancelApprove(proposal: Proposal) = Redis.pool.withClient {
    implicit client =>
      val tx = client.multi()
      tx.srem(s"ApprovedById:${conferenceId}:", proposal.id.toString)
      tx.srem(s"Approved:${conferenceId}:" + proposal.talkType.id, proposal.id.toString)
      tx.srem(s"ApprovedSpeakers:${conferenceId}:" + proposal.mainSpeaker, proposal.id.toString)

      proposal.secondarySpeaker.map {
        secondarySpeaker: String =>
          tx.srem(s"ApprovedSpeakers:${conferenceId}:" + secondarySpeaker, proposal.id.toString)
      }

      proposal.otherSpeakers.foreach {
        otherSpeaker: String =>
          tx.srem(s"ApprovedSpeakers:${conferenceId}:" + otherSpeaker, proposal.id.toString)
      }
      tx.exec()
  }

  def cancelRefuse(proposal: Proposal) = Redis.pool.withClient {
    implicit client =>
      val tx = client.multi()
      tx.srem(s"RefusedById:${conferenceId}:", proposal.id.toString)
      tx.srem(s"Refused:${conferenceId}:" + proposal.talkType.id, proposal.id.toString)
      tx.srem(s"RefusedSpeakers:${conferenceId}:" + proposal.mainSpeaker, proposal.id.toString)

      proposal.secondarySpeaker.map {
        secondarySpeaker: String =>
          tx.srem(s"RefusedSpeakers:${conferenceId}:" + secondarySpeaker, proposal.id.toString)
      }

      proposal.otherSpeakers.foreach {
        otherSpeaker: String =>
          tx.srem(s"RefusedSpeakers:${conferenceId}:" + otherSpeaker, proposal.id.toString)
      }
      tx.exec()
  }

  def allRefusedSpeakerIDs(): Set[String] = Redis.pool.withClient {
    implicit client =>
      client.keys(s"RefusedSpeakers:${conferenceId}:*").map {
        key =>
          val speakerUUID = key.substring(s"RefusedSpeakers:${conferenceId}:".length)
          speakerUUID
      }
  }

  def onlySubmittedRefused(): Iterable[Proposal] = Redis.pool.withClient {
    implicit client =>
      val proposalIDs = client.sinter(s"Proposals:$conferenceId:ByState:${ProposalState.SUBMITTED.code}", s"RefusedById:${conferenceId}:")
      Proposal.loadAndParseProposals(proposalIDs).values
  }

  def onlySubmittedNotRefused(): Iterable[Proposal] = Redis.pool.withClient {
    implicit client =>
      val proposalIDs = client.sdiff(s"Proposals:$conferenceId:ByState:${ProposalState.SUBMITTED.code}", s"RefusedById:${conferenceId}:", s"ApprovedById:${conferenceId}:")
      Proposal.loadAndParseProposals(proposalIDs).values
  }

  def allApprovedByTalkType(talkType: String): List[Proposal] = Redis.pool.withClient {
    implicit client =>
      val allProposalIDs = client.smembers(s"Approved:${conferenceId}:" + talkType).diff(client.smembers(s"Proposals:$conferenceId:ByState:${ProposalState.ARCHIVED.code}"))
      val allProposalWithVotes = Proposal.loadAndParseProposals(allProposalIDs.toSet)
      allProposalWithVotes.values.toList
  }

  def allRefusedByTalkType(talkType: String): List[Proposal] = Redis.pool.withClient {
    implicit client =>
      val allProposalIDs = client.smembers(s"Refused:${conferenceId}:" + talkType).diff(client.smembers(s"Proposals:$conferenceId:ByState:${ProposalState.ARCHIVED.code}"))
      val allProposalWithVotes = Proposal.loadAndParseProposals(allProposalIDs.toSet)
      allProposalWithVotes.values.toList
  }

  /**
  * Returns all proposals with the state Backup that are also not in the pseudo state
  * PreApproved or PreRefused
  */
  def allBackup():List[Proposal] = Redis.pool.withClient {
    implicit client =>
	    val keys = s"Proposals:$conferenceId:ByState:backup" +: (client.keys(s"Approved:${conferenceId}:*") ++: client.keys(s"Refused:${conferenceId}:*")).toSeq
	    val allProposalIDs = client.sdiff(keys: _*).asScala.toSet
      val allProposalWithVotes = Proposal.loadAndParseProposals(allProposalIDs)
      allProposalWithVotes.values.toList
  }

  /**
   * Approved = a proposal was selected by the program committee
   * Accepted = the speaker accepted to present the approved talk
   */
  def allApproved(): Set[Proposal] = Redis.pool.withClient {
    implicit client =>
      val allKeys = client.keys(s"Approved:${conferenceId}:*")
      val finalList = allKeys.map {
        key =>
          val allProposalIDs = client.smembers(key).diff(client.smembers(s"Proposals:$conferenceId:ByState:${ProposalState.ARCHIVED.code}")).toList
          val allProposalWithVotes = Proposal.loadAndParseProposals(allProposalIDs.toSet)
          allProposalWithVotes.values.toList
      }.flatten
      finalList
  }

  def allRefused(): Set[Proposal] = Redis.pool.withClient {
    implicit client =>
      val allKeys = client.keys(s"Refused:${conferenceId}:*")
      val finalList = allKeys.map {
        key =>
          val allProposalIDs = client.smembers(key).diff(client.smembers(s"Proposals:$conferenceId:ByState:${ProposalState.ARCHIVED.code}")).toList
          val allProposalWithVotes = Proposal.loadAndParseProposals(allProposalIDs.toSet)
          allProposalWithVotes.values.toList
      }.flatten
      finalList
  }

  def allApprovedProposalIDs() = Redis.pool.withClient {
    implicit client =>
      client.smembers(s"ApprovedById:${conferenceId}:")
  }

  def allRefusedProposalIDs() = Redis.pool.withClient {
    implicit client =>
      client.smembers(s"RefusedById:${conferenceId}:")
  }

  def allApprovedSpeakers(): Set[Speaker] = Redis.pool.withClient {
    implicit client =>
      client.keys(s"ApprovedSpeakers:${conferenceId}:*").flatMap {
        key =>
          val speakerUUID = key.substring(s"ApprovedSpeakers:${conferenceId}:".length)
          for (speaker <- Speaker.findByUUID(speakerUUID)) yield speaker
      }
  }

  def allApprovedSpeakerIDs(): Set[String] = Redis.pool.withClient {
    implicit client =>
      client.keys(s"ApprovedSpeakers:${conferenceId}:*").map {
        key =>
          val speakerUUID = key.substring(s"ApprovedSpeakers:${conferenceId}:".length)
          speakerUUID
      }
  }

  // Talks approved by the program committee
  def allApprovedTalksForSpeaker(speakerId: String): Iterable[Proposal] = Redis.pool.withClient {
    implicit client =>
      val allApprovedProposals = client.smembers(s"ApprovedSpeakers:${conferenceId}:" + speakerId)
      val mapOfProposals = Proposal.loadAndParseProposals(allApprovedProposals)
      mapOfProposals.values
  }

  // Talks for wich speakers confirmed he will present
  def allAcceptedTalksForSpeaker(speakerId: String): Iterable[Proposal] = {
    allApprovedTalksForSpeaker(speakerId).filter(_.state == ProposalState.ACCEPTED).toList
  }

  def allAcceptedByTalkType(talkType: String): List[Proposal] = Redis.pool.withClient {
    implicit client =>
      val allProposalIDs = client.smembers(s"Approved:${conferenceId}:" + talkType)
      val allProposalWithVotes = Proposal.loadAndParseProposals(allProposalIDs.toSet)
      allProposalWithVotes.values.filter(_.state == ProposalState.ACCEPTED).toList
  }

  def allApprovedSpeakersWithFreePass(): Set[Speaker] = Redis.pool.withClient {
    implicit client =>
      val allSpeakers = client.keys(s"ApprovedSpeakers:${conferenceId}:*").flatMap {
        key =>
          val speakerUUID = key.substring(s"ApprovedSpeakers:${conferenceId}:".length)
          for (speaker <- Speaker.findByUUID(speakerUUID)) yield {
            (speaker,
              Proposal.loadAndParseProposals(client.smembers(key)).values.filter(p => ConferenceDescriptor.ConferenceProposalConfigurations.doesItGivesSpeakerFreeEntrance(p.talkType))
              )
          }
      }
      val setOfSpeakers = allSpeakers.filterNot(_._2.isEmpty).map(_._1)
      setOfSpeakers
  }
}

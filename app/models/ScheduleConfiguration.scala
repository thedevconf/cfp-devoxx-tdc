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
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsPath, Json}
import org.apache.commons.lang3.RandomStringUtils
import scala.util.Random
import org.joda.time.{DateTimeZone, DateTime}

/**
 * Slots that are scheduled.
 *
 * Created by Nicolas Martignole on 27/02/2014.
 */
case class TimeSlot(start: DateTime, end: DateTime) {
  override def hashCode(): Int = {
    start.hashCode() + end.hashCode()
  }

  override def equals(obj: scala.Any): Boolean = {
    if (obj.isInstanceOf[TimeSlot]) {
      val d2 = obj.asInstanceOf[TimeSlot]
      d2.start.equals(this.start) && d2.end.equals(this.end)
    } else {
      false
    }
  }
}

case class ScheduleConfiguration(confType: String, slots: List[Slot], timeSlots: List[TimeSlot])

case class ScheduleSaved(id: String, confType: String, createdBy: String, hashCodeSlots: Int)

object ScheduleConfiguration {
  implicit val timeSlotFormat = Json.format[TimeSlot]
  implicit val scheduleConfFormat = Json.format[ScheduleConfiguration]
  implicit val scheduleSavedFormat = Json.format[ScheduleSaved]

  def persist(confType: String, slots: List[Slot], createdBy: Webuser):String = Redis.pool.withClient {
    implicit client =>
      val id = confType + "-" + (RandomStringUtils.randomAlphabetic(3) + "-" + RandomStringUtils.randomNumeric(2)).toLowerCase
      val key = ScheduleSaved(id, confType, createdBy.firstName, slots.hashCode())
      val jsonKey = Json.toJson(key).toString()
      client.zadd("ScheduleConfiguration", System.currentTimeMillis() / 1000, jsonKey)

      val timeSlots = slots.map {
        slot =>
          TimeSlot(slot.from, slot.to)
      }.sortBy(_.start.getMillis)

      val config = ScheduleConfiguration(confType, slots, timeSlots)
      val json = Json.toJson(config).toString()
      client.hset("ScheduleConfigurationByID", id, json)
      id
  }

  def delete(id: String) = Redis.pool.withClient {
    implicit client =>
      val scheduledSlotsKey = client.zrevrangeWithScores("ScheduleConfiguration", 0, -1)
      val tx = client.multi()

      scheduledSlotsKey.map {
        case (key: String, _) =>
          val scheduleSaved = Json.parse(key).as[ScheduleSaved]
          if (scheduleSaved.id == id) {
            tx.zrem("ScheduleConfiguration", key)
          }
      }
      tx.hdel("ScheduleConfigurationByID", id)
      tx.exec()
  }

  def allScheduledConfigurationWithLastModified(): List[(String, Double)] = Redis.pool.withClient {
    implicit client =>
      client.zrevrangeWithScores("ScheduleConfiguration", 0, -1)
  }

  def loadScheduledConfiguration(id: String): Option[ScheduleConfiguration] = Redis.pool.withClient {
    implicit client =>
      client.hget("ScheduleConfigurationByID", id).flatMap {
        json: String =>
          val maybeScheduledConf = Json.parse(json).validate[ScheduleConfiguration]
          maybeScheduledConf.fold(errors => {
            play.Logger.of("models.ScheduledConfiguration").warn("Unable to reload a SlotConfiguration due to JSON error");
            play.Logger.of("models.ScheduledConfiguration").warn(s"Got error : ${library.ZapJson.showError(errors)} ");
            None
          }
            , someConf => Option(someConf)
          )
      }
  }

  def publishConf(id: String, confType: String) = Redis.pool.withClient {
    implicit client =>
      client.hset("Published:Schedule", confType, id)
  }

  def getPublishedSchedule(confType: String): Option[String] = Redis.pool.withClient {
    implicit client =>
      client.hget("Published:Schedule", confType)
  }

  def getPublishedScheduleByDay(day: String): List[Slot] = {

    def extractSlot(allSlots: List[Slot], day: String) = {
      val configured = loadSlots().filter(_.day == day)
      val configuredIDs = configured.map(_.id)
      val filtered = allSlots.filterNot(s => configuredIDs.contains(s.id))
      configured ++ filtered
    }

    val listOfSlots = day match {
      case "wednesday" =>
        extractSlot(ConferenceDescriptor.ConferenceSlots.wednesday, "wednesday")
      case "thursday" =>
        extractSlot(ConferenceDescriptor.ConferenceSlots.thursday, "thursday")
      case "friday" =>
        extractSlot(ConferenceDescriptor.ConferenceSlots.friday, "friday")
      case other =>
        play.Logger.of("ScheduleConfiguration").warn("Could not match " + other + " in getPublishedScheduleByDay")
        Nil
    }

    listOfSlots.sortBy(_.from.getMillis)
  }

  def loadSlots(): List[Slot] = {
    ConferenceDescriptor.ConferenceProposalTypes.ALL.flatMap {
      t: ProposalType => loadSlotsForConfType(t.id)
    }
  }

  def loadSlotsForConfType(confType: String): List[Slot] = {
    getPublishedSchedule(confType).flatMap {
      id: String =>
        loadScheduledConfiguration(id).map {
          scheduledConf =>
            scheduledConf.slots
        }
    }.getOrElse(List.empty[Slot])
  }

  // Retrieve the time slot for a specific proposalId
  def findSlotForConfType(confType: String, proposalId: String): Option[Slot] = {
    loadSlotsForConfType(confType).filter(_.proposal.isDefined).filter(_.proposal.get.id == proposalId).headOption
  }

  def loadAllConfigurations() = {
    val allConfs = for (confType <- ProposalType.allIDsOnly;
                        slotId <- ScheduleConfiguration.getPublishedSchedule(confType);
                        configuration <- ScheduleConfiguration.loadScheduledConfiguration(slotId)
    ) yield configuration

    allConfs
  }

  def loadAllPublishedSlots():List[Slot]={
    loadAllConfigurations().map{
      sc=>
        sc.slots
    }.flatten.toList
  }


  def  loadNextTalks() = {
    val allAgendas = ScheduleConfiguration.loadAllConfigurations()
    val slots = allAgendas.map(_.slots).flatten
    Option(slots.filter(_.from.isAfter(new DateTime().toDateTime(DateTimeZone.forID("America/Sao_Paulo")))).sortBy(_.from.toDate.getTime).take(10))
  }

  def loadRandomTalks() = {
    val allAgendas = ScheduleConfiguration.loadAllConfigurations()
    val slots = allAgendas.map(_.slots).flatten
    Option(Random.shuffle(slots.toList).take(10))
  }

}

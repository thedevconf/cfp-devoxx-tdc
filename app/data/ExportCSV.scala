package data

import play.api.Play.current
import library.Redis
import play.api.libs.json.Json
import redis.clients.jedis._

import scala.collection.JavaConverters._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import java.io._

import org.apache.commons.csv._
import java.nio.file.Files

object ExportCSV extends App {

  val host = sys.env("REDIS_HOST")
  val port = Integer.valueOf(sys.env("REDIS_PORT"))
  val password = sys.env("REDIS_PASSWORD")
  val limit = 1000

  type DB = Map[String, Table]
  type Table = Map[String, Row]
  type Row = Map[Int,String]

  def ingestTokens(world: DB, tokens: String) = {
    val tableName = tokens.split(":").head
    if (!world.contains(tableName)) {
      world.put(tableName, collection.mutable.Map[String,Row]())
    }
    /*
    val table = db(tableName)
    val key = tokens.tail.head
    val data = tokens.slice(1,tokens.size)
    val ldata = data.toList

    val row = table.get(key).getOrElse(scala.collection.mutable.ListBuffer())
    row ++= ldata
    table(key) = row

     */
  }

  def getProposalsTable(db: DB) = {
    if (! db.contains("Proposals") ){
      val table = collection.mutable.Map[String,Row]()
      db += ("Proposals" -> table)
    }
    db("Proposals")
  }

  val KEY_SEP = "_"
  val PROPOSAL_EVENT = 0
  val PROPOSAL_CODE = 1
  val PROPOSAL_AUTHOR = 2
  val PROPOSAL_TRACKS = 3

  def getProposalRow(db:DB, eventCode: String, proposalCode: String) = {
    val table = getProposalsTable(db)
    val key = eventCode + KEY_SEP + proposalCode
    if (! table.contains(key)){
      val row = collection.mutable.Map[Int,String]()
      row(PROPOSAL_EVENT) = eventCode
      row(PROPOSAL_CODE) = proposalCode
      row(PROPOSAL_AUTHOR) = "?AUTHOR?"
      row(PROPOSAL_TRACKS) = "" 
      table += ( key -> row)
    }
    table(key)
  }

  def ingestProposalByAuthor(jedis:Jedis, db: DB, key: String) = {
    val tokens = key.split(":")
    if (tokens.size == 4 && tokens(2) == "ByAuthor" ){
      // println(s"OK Proposals:EVENT:ByAuthor:AUTHOR_UUUID [${key}]")
      val eventCode = tokens(1)
      val authorUUID = tokens(3)
      val smembers = jedis.smembers(key).asScala
      for (proposalCode <- smembers){
        val proposalRow = getProposalRow(db,eventCode,proposalCode)
        proposalRow(PROPOSAL_AUTHOR) = authorUUID
      }
    } else if (tokens.size == 3){
      ignore()
    } else {
      println(s"??? Proposals [${key}]")
    }
  }

  def ingestProposalByTrack(jedis:Jedis, db: DB, key: String) = {
    val tokens = key.split(":")
    if(tokens.size == 4){
      val eventCode = tokens(1)
      val trackCode = tokens(3)
      val smembers = jedis.smembers(key).asScala
      for (proposalCode <- smembers){
        val proposalRow = getProposalRow(db,eventCode,proposalCode)
        val proposalTracks = proposalRow.get(PROPOSAL_TRACKS).getOrElse(null)
        if (proposalTracks.size > 0){
          proposalRow(PROPOSAL_TRACKS) = s"$proposalTracks $trackCode"
        }else {
          proposalRow(PROPOSAL_TRACKS) = s"$trackCode"
        }
      }
    }else {
      println(s"? Proposals:EVENT_CODE:ByTrack... [$key]")
    }
  }

  def ignore() = {}

  def ingestProposal(jedis:Jedis, db: DB, key: String) =
    key match {
      case p if key.contains(":Dates:") => ignore() //zset of ?uuid
      case p if key.contains(":Reviewed:ByProposal:") => ignore() //set of ?uuid
      case p if key.contains(":Reviewed:ByAuthor:") => ignore() //set of ?uuid
      case p if key.contains(":Votes:") => ignore() //set of ?uuid
      case p if key.contains(":ByAuthor:") => ingestProposalByAuthor(jedis, db , key)
      case p if key.contains(":ByTrack:") => ingestProposalByTrack(jedis, db , key)
      case _ => println(s"PROPOSAL? [$key]")
  }

  def ingest(jedis:Jedis, db:DB, key:String) =
      key match {
        case p if key.startsWith("Proposals:") => ingestProposal(jedis, db,p)
        //case "Events" => ingestEvents(db,tokens)
        case h: String => ingestTokens(db,key)
      }


  def export(world:DB): Unit = {
    val home = sys.env("HOME")
    val dataDir = s"$home/.cfp/"
    val dataDirFile = new java.io.File(dataDir)
    val dataDirPath = dataDirFile.toPath
    if (! dataDirFile.exists())
      dataDirFile.mkdirs()
    for ((tableName, table) <- world) {
      println(s"TABLE [$tableName]")
      val csvPath = dataDirPath.resolve(s"${tableName}.csv")
      if (Files.exists(csvPath))
        Files.delete(csvPath)
      val writer = new FileWriter(csvPath.toFile)
      val printer = new CSVPrinter(writer, CSVFormat.DEFAULT)
      for ( (key, row) <- table ){
        val sorted = row.toSeq.sortBy(_._1)
        val iterable = sorted.map(_._2).asJava
        val csvRow:java.lang.Iterable[String] = iterable
        printer.printRecord(csvRow)
      }
      printer.close()
      writer.close()
      /*
      for ((key,row) <- table) {
        println(s"ROW[$key] = [$row]")
      }
      */

    }
  }

  def exportCSV() = {
    println(s"Exporting data from redis [${host}:${port}]...")
    val db = collection.mutable.Map[String,Table]()
    val jedis = new Jedis(host, port )
    jedis.auth(password)
    val keys_java = jedis.keys("*")
    val keys_set = keys_java.asScala 
    val keys = keys_set.toList
    var index = 0
    var done = false

    while (! done) {
      val key = keys(index)
      ingest(jedis,db,key)
      index += 1
      done = index == keys.size || index == limit
    }
    println(s"Ingested [$index] records")
    jedis.quit()
    export(db)
  }

  exportCSV()
}

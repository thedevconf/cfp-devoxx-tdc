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

  def getProposalRow(table: Table, eventCode: String, proposalCode: String) = {
    val key = eventCode + KEY_SEP + proposalCode
    if (! table.contains(key)){
      val row = collection.mutable.Map[Int,String]()
      table += ( key -> row)
    }
    table(key)
  }

  def ingestProposalByAuthor(jedis:Jedis, db: DB, key: String) = {
    val tokens = key.split(":")
    if (tokens.size == 4){
      println(s"Proposals:EVENT:ByAuthor:AUTHOR_UUUID [${key}]")
      val eventCode = tokens(1)
      val authorUUID = tokens(3)
      val table = getProposalsTable(db)
      val smembers = jedis.smembers(key).asScala
      for (proposalCode <- smembers){
        val proposalRow = getProposalRow(table,eventCode,proposalCode)
        proposalRow(PROPOSAL_AUTHOR) = authorUUID
      }
    }
  }

  def ingestProposal(jedis:Jedis, db: DB, key: String) =
    key match {
      case p if key.contains(":ByAuthor:") => ingestProposalByAuthor(jedis, db , key)
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
        val csvRow:java.lang.Iterable[String] = row.values.asJava
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

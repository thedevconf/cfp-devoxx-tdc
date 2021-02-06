package library

import java.io.File

import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.PutObjectRequest
import com.amazonaws.services.s3.model.CannedAccessControlList
import com.amazonaws.services.s3.model.ListObjectsV2Result
import com.amazonaws.services.s3.model.S3ObjectSummary
import play.api.Play

import collection.JavaConverters._

object S3 {

  val bucketName = Play.current.configuration.getString("s3.bucket").getOrElse("thedevconf")
  val bucketRegion = Play.current.configuration.getString("s3.region").getOrElse("sa-east-1")
  
  val s3 =  AmazonS3ClientBuilder.standard()
      .withRegion(bucketRegion)
      .build()
  
  val presentationSourceDir = "/tmp/presentations/"
  val pictureSourceDir = "/tmp/photos/"

  def uploadPresentation(eventCode:String, trackId:String, filename:String) = {
    val key = s"presentations/$eventCode/$trackId/$filename"
    val putObject = new PutObjectRequest(bucketName,key,new File(presentationSourceDir + filename))
                        .withCannedAcl(CannedAccessControlList.PublicRead)
    s3.putObject(putObject)
    s"https://s3-${bucketRegion}.amazonaws.com/$bucketName/$key"
  }
  
  def uploadPicture(userId:String, filename:String):String = {
    val extension = filename.substring(filename.indexOf(".")).toLowerCase
    val key = s"photos/$userId$extension"
    val putObject = new PutObjectRequest(bucketName,key,new File(pictureSourceDir + filename))
                        .withCannedAcl(CannedAccessControlList.PublicRead)
    s3.putObject(putObject)
    s"https://s3-${bucketRegion}.amazonaws.com/$bucketName/$key"
  }
  
  /**
   * returns the URLS for the uploaded presentations for the specified event
   */
  def getUploadedPresentations(eventCode:String):List[String] = {
	val result:ListObjectsV2Result = s3.listObjectsV2(bucketName,s"presentations/$eventCode")
	val objects = result.getObjectSummaries().asScala
	objects.map(o => s"https://s3-${bucketRegion}.amazonaws.com/$bucketName/${o.getKey}").toList
  }
}

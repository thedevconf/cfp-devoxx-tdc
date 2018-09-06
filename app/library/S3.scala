package library

import java.io.File

import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.PutObjectRequest
import com.amazonaws.services.s3.model.CannedAccessControlList

object S3 {

  val bucketName = "thedevconf"
  val presentationSourceDir = "/tmp/presentations/"
  val pictureSourceDir = "/tmp/photos/"

  def uploadPresentation(eventCode:String, trackId:String, filename:String) = {
    val s3 = AmazonS3ClientBuilder.defaultClient()
    val key = s"presentations/$eventCode/$trackId/$filename"
	val putObject = new PutObjectRequest(bucketName,key,new File(presentationSourceDir + filename))
						.withCannedAcl(CannedAccessControlList.PublicRead)
    s3.putObject(putObject)
  }
  
  def uploadPicture(userId:String, filename:String):String = {
    val s3 = AmazonS3ClientBuilder.defaultClient()
	val extension = filename.substring(filename.indexOf(".")).toLowerCase
    val key = s"photos/$userId$extension"
	val putObject = new PutObjectRequest(bucketName,key,new File(pictureSourceDir + filename))
						.withCannedAcl(CannedAccessControlList.PublicRead)
    s3.putObject(putObject)
	s"https://s3-sa-east-1.amazonaws.com/$bucketName/$key"
  }
}

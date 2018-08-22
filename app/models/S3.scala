package models

import java.io.File

import com.amazonaws.services.s3.AmazonS3ClientBuilder

object S3 {

  val bucketName = "thedevconf"
  val presentationSourceDir = "/tmp/presentations/"

  def uploadPresentation(eventCode:String, trackId:String, filename:String) = {
    val s3 = AmazonS3ClientBuilder.defaultClient()
    val key = s"presentations/$eventCode/$trackId/$filename"
    s3.putObject(bucketName,key,new File(presentationSourceDir + filename))
  }
}

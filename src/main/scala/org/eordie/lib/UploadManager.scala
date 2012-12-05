package org.eordie.lib

import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.common._
import net.liftweb.util.StringHelpers
import com.mongodb.gridfs.{GridFSInputFile, GridFSDBFile}
import com.mongodb.BasicDBObject
import net.liftweb.http.BadResponse
import net.liftweb.http.StreamingResponse
import net.liftweb.http.InMemoryResponse
import org.eordie.config.MongoConfig
import org.eordie.model.car.Offer
import net.liftweb.util.ControlHelpers._
import net.liftweb.http.StreamingResponse
import net.liftweb.http.InMemoryResponse
import net.liftweb.common.Full
import net.liftweb.http.BadResponse

/**
 * Менеджер для загрузки файлов
 *
 * @author Alexandr Kolosov
 * @since 10/24/12
 */
object UploadManager extends RestHelper with Loggable {

  def clear(refs: List[String]) {
    refs.diff(Offer.findAll.flatMap(_.images.is)).foreach(clear(_))
  }

  def clear(ref: String) {
    logger.info("Removing image ref " + ref)
    MongoConfig.mongoGridFS.remove(ref)
  }

  serve {
    case "uploading" :: Nil Post req => {
      def saveImage(fph: FileParamHolder) = {
        val imageName = StringHelpers.randomString(16)
        val file: GridFSInputFile = MongoConfig.mongoGridFS.createFile(fph.fileStream, imageName)

        file.setMetaData(new BasicDBObject().append("originalFileName", fph.fileName))
        file.setContentType(fph.mimeType)
        file.save()

        ThumbnailGenerator ! file

        ("name" -> imageName) ~ ("type" -> fph.mimeType) ~ ("size" -> fph.length)
      }

      val ojv: Box[JValue] = req.uploadedFiles.map(fph => saveImage(fph)).headOption
      val ajv = ("name" -> "n/a") ~ ("type" -> "n/a") ~ ("size" -> 0L)
      val ret = ojv openOr ajv

      val jr = JsonResponse(ret).toResponse.asInstanceOf[InMemoryResponse]
      InMemoryResponse(jr.data,
        ("Content-Length", jr.data.length.toString) ::
        ("Content-Type", "text/plain") :: Nil,
        Nil, 200)
    }

    case "remove" :: imageName :: Nil Get req => {
      clear(imageName)
      JsonResponse("status" -> true).toResponse.asInstanceOf[InMemoryResponse]
    }

    case "download" :: imageName :: Nil Get req => {
      stream(imageName, (image: GridFSDBFile) => {
        val originalFileName = image.getMetaData.get("originalFileName").toString
        List(("Content-Disposition", "attachment; filename=" + originalFileName))
      })
    }

    case "serving" :: imageName :: Nil Get req => {
      stream(imageName, (image: GridFSDBFile) => List(("Content-Type", image.getContentType)))
    }
  }

  def stream(imageName: String, contentTypeFunc: (GridFSDBFile) => List[(String, String)]): () => Box[LiftResponse] = {
    tryo(MongoConfig.mongoGridFS.findOne(imageName)) match {
      case Full(image) => {
        val imageStream = image.getInputStream
        StreamingResponse(imageStream, () => imageStream.close(), image.getLength, contentTypeFunc(image), Nil, 200)
      }

      case Empty => new BadResponse

      case Failure(_, ex, _) => {
        logger.info("Can't find image", ex.openTheBox)
        new BadResponse
      }
    }
  }
}
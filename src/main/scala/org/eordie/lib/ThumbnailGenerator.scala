package org.eordie.lib

import net.liftweb.actor.SpecializedLiftActor
import org.eordie.config.MongoConfig
import com.mongodb.gridfs.{GridFSInputFile, GridFSDBFile}
import net.coobird.thumbnailator.Thumbnails
import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import net.liftweb.common.Loggable
import com.sun.xml.internal.messaging.saaj.util.ByteInputStream

/**
 * Создает иконки для загруженных картинок
 *
 * @author Alexandr Kolosov
 * @since 11/19/12
 */
object ThumbnailGenerator extends SpecializedLiftActor[GridFSInputFile] with Loggable {

  protected def messageHandler = {
    case inputFile: GridFSInputFile => {
      val filename: String = inputFile.getFilename
      logger.info("Getting file:" + filename)

      val file: GridFSDBFile = MongoConfig.mongoGridFS.findOne(filename)
      val thumbOutStream: ByteArrayOutputStream = new ByteArrayOutputStream()

      logger.info("Working with file:" + file.getFilename)
      Thumbnails.of(file.getInputStream).size(100, 100).toOutputStream(thumbOutStream)
      val dbFile: GridFSInputFile = MongoConfig.mongoGridFS.createFile(thumbOutStream.toByteArray)
      dbFile.setFilename(filename + "_small")
      dbFile.setContentType(inputFile.getContentType)
      dbFile.save()

      logger.info("New thumbnail with file name " + dbFile.getFilename)
    }
  }
}

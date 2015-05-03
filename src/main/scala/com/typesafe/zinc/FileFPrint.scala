/**
 * Copyright (C) 2012 Typesafe, Inc. <http://www.typesafe.com>
 */

package com.typesafe.zinc

import java.io.{FileNotFoundException, File}
import com.google.common.hash.Hashing
import com.google.common.base.Charsets


class FileFPrint(val file: File, val fprint: String) {
  override def hashCode = fprint.hashCode

  override def equals(o: Any) = o match {
    case that: FileFPrint => fprint == that.fprint
    case _ => false
  }

  override def toString = "(%s: %s)".format(fprint, file.getPath)
}

object FileFPrint {
  private val HashFunction = Hashing.murmur3_128()
  private val LongStringLen = (2l^31).toString.size

  def fprint(file: File): Option[FileFPrint] = {
    try {
      if (!file.exists()) {
        return None
      }
      val filePath = file.getCanonicalPath()
      val hasher = HashFunction.newHasher(2 * (LongStringLen + filePath.size))
      hasher.putString(filePath, Charsets.UTF_8)
      hasher.putLong(file.lastModified)
      Some(new FileFPrint(file, hasher.hash.toString))
    } catch {
      case e: FileNotFoundException => None
    }
  }
}

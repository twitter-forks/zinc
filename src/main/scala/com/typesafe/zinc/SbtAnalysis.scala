/**
 * Copyright (C) 2012 Typesafe, Inc. <http://www.typesafe.com>
 */

package com.typesafe.zinc

import java.io.File
import java.net.URL
import java.nio.charset.Charset
import sbt.compiler.CompileOutput
import sbt.inc.{ APIs, Analysis, Relations, SourceInfos, Stamps }
import sbt.{ CompileSetup, Logger, Relation, Using }
import xsbti.compile.SingleOutput


object SbtAnalysis {

  /**
   * Run analysis manipulation utilities.
   */
  def runUtil(util: AnalysisUtil, log: Logger,
              mirrorAnalysis: Boolean = false,
              cwd: Option[File] = None): Unit = {
    runMerge(util.cache, util.merge, mirrorAnalysis, cwd)
    runRebase(util.cache, util.rebase, mirrorAnalysis, cwd)
    runSplit(util.cache, util.split, mirrorAnalysis, cwd)
    runReload(util.reload)
  }

  /**
   * Run an analysis merge. The given analyses should share the same compile setup.
   * The merged analysis will overwrite whatever is in the combined analysis cache.
   */
  def runMerge(combinedCache: Option[File], cacheFiles: Seq[File],
               mirrorAnalysis: Boolean = false,
               cwd: Option[File] = None): Unit = {
    if (cacheFiles.nonEmpty) {
      combinedCache match {
        case None => throw new Exception("No cache file specified")
        case Some(cacheFile) =>
          val analysisStore = Compiler.analysisStore(cacheFile)
          mergeAnalyses(cacheFiles) match {
            case Some((mergedAnalysis, mergedSetup)) => {
                analysisStore.set(mergedAnalysis, mergedSetup)
                if (mirrorAnalysis) {
                  printRelations(mergedAnalysis, Some(new File(cacheFile.getPath() + ".relations")), cwd)
                }
              }
            case None =>
          }
      }
    }
  }

  /**
   * Merge analyses and setups into one analysis and setup.
   * Currently the compile setups are not actually merged, last one wins.
   */
  def mergeAnalyses(cacheFiles: Seq[File]): Option[(Analysis, CompileSetup)] = {
    val analysesAndSetups: Seq[(Analysis, CompileSetup)] = cacheFiles flatMap { Compiler.analysisStore(_).get() }
    val mergedAnalysis = Analysis.merge(analysesAndSetups map {_._1})
    analysesAndSetups.lastOption map { x: (Analysis, CompileSetup) => (mergedAnalysis, x._2) }
  }

  /**
   * Run an analysis rebase. Rebase all paths in the analysis, and the output directory
   * in the compile setup.
   */
  def runRebase(cache: Option[File], rebase: Map[File, File],
                mirrorAnalysis: Boolean, cwd: Option[File]): Unit = {
    if (!rebase.isEmpty) {
      cache match {
        case None => throw new Exception("No cache file specified")
        case Some(cacheFile) =>
          val analysisStore = Compiler.analysisStore(cacheFile)
          analysisStore.get match {
            case None => throw new Exception("No analysis cache found at: " + cacheFile)
            case Some((analysis, compileSetup)) => {
              implicit val fileMapper: Mapper[File] = createMultiRebasingFileMapper(rebase)
              implicit val urlMapper: Mapper[URL] = createURLMapper(fileMapper)
              val newAnalysis = rebaseAnalysis(analysis)
              val newSetup = rebaseSetup(compileSetup)
              analysisStore.set(newAnalysis, newSetup)
              if (mirrorAnalysis) {
                printRelations(newAnalysis, Some(new File(cacheFile.getPath() + ".relations")), cwd)
              }
            }
          }
      }
    }
  }

  trait Mapper[T] {
    def apply(u: T): Option[T]
  }

  /**
   * Create a mapper function that performs multiple rebases. For a given file, it uses the first rebase
   * it finds in which the source base is a prefix of the file path. If no matching rebase is found, it
   * returns the original path unchanged.
   *
   * The order of rebases is undefined, so it's highly recommended that there never be two
   * rebases A1->B1, A2->B2 such that A1 is a prefix of A2.
   *
   * Note that this doesn't need to do general-purpose relative rebasing for paths with ../ etc. So it
   * uses a naive prefix-matching algorithm.
   */
  def createMultiRebasingFileMapper(rebase: Map[File, File]): Mapper[File] = {
    def createSingleRebaser(fromBase: String, toBase: Option[String]): PartialFunction[String, Option[String]] = {
      case path if path.startsWith(fromBase) => { toBase.map(_ + path.substring(fromBase.length)) }
    }

    val rebasers: List[PartialFunction[String, Option[String]]] =
      (rebase map { x: (File, File) =>
        createSingleRebaser(x._1.getAbsolutePath, if (x._2.getPath.isEmpty) None else Some(x._2.getAbsolutePath))
      }).toList

    val multiRebaser: PartialFunction[String, Option[String]] =
      rebasers reduceLeft (_ orElse _) orElse { case s: String => Some(s) }

    new Mapper[File] {
      def apply(f: File) =
        multiRebaser(f.getAbsolutePath) map { new File(_) }
    }
  }

  def createURLMapper(mapper: Mapper[File]): Mapper[URL] = new Mapper[URL] {
    def apply(u: URL) = mapURLFile(u)(mapper.apply)
  }

  /** FIXME: XXX: tests. */
  def mapURLFile(u: URL)(f: File=>Option[File]): Option[URL] =
    u.getProtocol match {
      case "jar" =>
        // rebase only the jar portion
        val Array(jarFile, content) = u.getFile.split("!/", 2)
        f(new File(u.getFile)).map { mappedFile =>
          new URL("jar:file:" + mappedFile + "!/" + content)
        }
      case "file" =>
        f(new File(u.getFile)).map(_.toURI.toURL)
    }

  /**
   * Rebase all paths in an analysis.
   */
  def rebaseAnalysis(analysis: Analysis)(implicit fmapper: Mapper[File], umapper: Mapper[URL]): Analysis = {
    analysis.copy(rebaseStamps(analysis.stamps), rebaseAPIs(analysis.apis),
      rebaseRelations(analysis.relations), rebaseInfos(analysis.infos))
  }

  def rebaseStamps(stamps: Stamps)(implicit fmapper: Mapper[File], umapper: Mapper[URL]): Stamps = {
    Stamps(
      rebaseMap(stamps.products),
      rebaseMap(stamps.sources),
      rebaseMap(stamps.binaries),
      rebaseMap(stamps.classNames)
    )
  }

  def rebaseAPIs(apis: APIs)(implicit mapper: Mapper[File]): APIs = {
    APIs(rebaseMap(apis.internal), apis.external)
  }

  def rebaseRelations(relations: Relations)(implicit fmapper: Mapper[File], umapper: Mapper[URL]): Relations = {
    Relations.make(
      rebaseRelation(relations.srcProd),
      rebaseRelation(relations.binaryDep),
      Relations.makeSource(
        rebaseRelation(relations.direct.internal),
        rebaseExtRelation(relations.direct.external)
      ),
      Relations.makeSource(
        rebaseRelation(relations.publicInherited.internal),
        rebaseExtRelation(relations.publicInherited.external)
      ),
      rebaseExtRelation(relations.classes)
    )
  }

  def rebaseInfos(infos: SourceInfos)(implicit mapper: Mapper[File]): SourceInfos = {
    SourceInfos.make(rebaseMap(infos.allInfos))
  }

  def rebaseRelation[K:Mapper, V:Mapper](relation: Relation[K, V]): Relation[K, V] = {
    def rebase[K:Mapper,V:Mapper](map: Map[K, Set[V]]) = rebaseSetMap(rebaseMap(map))
    Relation.make(rebase(relation.forwardMap), rebase(relation.reverseMap))
  }

  def rebaseExtRelation[K:Mapper](relation: Relation[K, String]): Relation[K, String] = {
    Relation.make(rebaseMap(relation.forwardMap), rebaseSetMap(relation.reverseMap))
  }

  def rebaseMap[T:Mapper,A](map: Map[T, A]): Map[T, A] = {
    val mapper = implicitly[Mapper[T]]
    map flatMap { case (f, a) => mapper(f) map { (_, a) } }
  }

  def rebaseSetMap[A,T:Mapper](fileSetMap: Map[A, Set[T]]): Map[A, Set[T]] = {
    val mapper = implicitly[Mapper[T]]
    fileSetMap mapValues { _ flatMap { f => mapper(f) } }
  }

  /**
   * Rebase the output directory of a compile setup.
   */
  def rebaseSetup(setup: CompileSetup)(implicit mapper: Mapper[File]): CompileSetup = {
    val output = Some(setup.output) collect { case single: SingleOutput => single.outputLocation }
    output flatMap mapper.apply map { dir => new CompileSetup(CompileOutput(dir), setup.options, setup.compilerVersion, setup.order, setup.nameHashing) } getOrElse setup
  }

  /**
   * Run an analysis split. The analyses are split by source directory and overwrite
   * the mapped analysis cache files.
   */
  def runSplit(cache: Option[File], mapping: Map[Seq[File], File],
              mirrorAnalysis: Boolean = false,
              cwd: Option[File] = None): Unit = {
    if (mapping.nonEmpty) {
      val expandedMapping: Map[File, File] = for {
        (srcs, analysis) <- mapping
        src <- srcs
      } yield (src, analysis)
      // A split with no specified source files acts as a "catch-all", for analysis
      // belonging to source files not specified on any other split.
      val catchAll: Option[File] = mapping.find( { _._1.isEmpty } ) map { _._2 }
      def discriminator(f: File): Option[File] = expandedMapping.get(f) match {
        case None => catchAll
        case s => s
      }
      cache match {
        case None => throw new Exception("No cache file specified")
        case Some(cacheFile) =>
          Compiler.analysisStore(cacheFile).get() match {
            case None => throw new Exception("No analysis cache found at: " + cacheFile)
            case Some ((analysis, compileSetup)) => {
              def writeAnalysis(cacheFile: File, analysis: Analysis) {
                Compiler.analysisStore(cacheFile).set(analysis, compileSetup)
                if (mirrorAnalysis) {
                  printRelations(analysis, Some(new File(cacheFile.getPath + ".relations")), cwd)
                }
              }
              val grouped: Map[Option[File], Analysis] = analysis.groupBy(discriminator)
              for ((splitCacheOpt, splitAnalysis) <- grouped) {
                splitCacheOpt match {
                  case Some(splitCache) => writeAnalysis(splitCache, splitAnalysis)
                  case None =>
                }
              }
              // Some groups may be empty, but we still want to write something out.
              val emptySplits: Set[File] = mapping.values.toSet -- grouped.keySet.flatten
              for (file <- emptySplits) {
                writeAnalysis(file, Analysis.Empty)
              }
            }
          }
      }
    }
  }

  /**
   * Run an analysis reload. The in-memory cache is updated from the specified file.
   */
  def runReload(cacheFiles: Seq[File]): Unit = {
    // TODO: Do we still need reload functionality now that we cache by fingerprint?
    for (cacheFile <- cacheFiles) {
      Compiler.analysisStore(cacheFile).get()
    }
  }

  /**
   * Print readable analysis outputs, if configured.
   */
  def printOutputs(analysis: Analysis, outputRelations: Option[File], outputProducts: Option[File], cwd: Option[File], classesDirectory: File): Unit = {
    printRelations(analysis, outputRelations, cwd)
    printProducts(analysis, outputProducts, classesDirectory)
  }

  /**
   * Print analysis relations to file.
   */
  def printRelations(analysis: Analysis, output: Option[File], cwd: Option[File]): Unit = {
    for (file <- output) {
      val userDir = (cwd getOrElse Setup.Defaults.userDir) + "/"
      Using.fileWriter(utf8)(file) { out =>
        def writeNoCwd(s: String) = if (s.startsWith(userDir)) out.write(s, userDir.length, s.length - userDir.length) else out.write(s)
        def printRelation(header: String, r: Relation[File, _]) {
          out.write(header + ":\n")
          r._1s.toSeq.sorted foreach { k =>
            r.forward(k).toSeq.map(_.toString).sorted foreach { v =>
              out.write("   "); writeNoCwd(k.toString); out.write(" -> "); writeNoCwd(v); out.write("\n")
            }
          }
        }
        import analysis.relations.{ srcProd, binaryDep, internalSrcDep, externalDep, classes }
        val sections =
          ("products", srcProd) ::
          ("binary dependencies", binaryDep) ::
          ("source dependencies", internalSrcDep) ::
          ("external dependencies", externalDep) ::
          ("class names", classes) ::
          Nil
        sections foreach { x => printRelation(x._1, x._2) }
      }
    }
  }

  /**
   * Print just source products to file, relative to classes directory.
   */
  def printProducts(analysis: Analysis, output: Option[File], classesDirectory: File): Unit = {
    for (file <- output) {
      Using.fileWriter(utf8)(file) { out =>
        def relativeFile(path: File): String = Util.relativize(classesDirectory, path)
        def relativeURL(u: URL): String =
          mapURLFile(u) { f =>
            Some(new File(relativeFile(f)))
          }.get.toString
        import analysis.relations.srcProd
        srcProd._1s.toSeq.sorted foreach {  k =>
          srcProd.forward(k).toSeq.sortBy(_.toString) foreach { v =>
            out.write(relativeFile(k)); out.write(" -> "); out.write(relativeURL(v)); out.write("\n")
          }
        }
      }
    }
  }

  private[this] val utf8 =  Charset.forName("UTF-8")
}

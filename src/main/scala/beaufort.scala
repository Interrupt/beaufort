import java.io.File
import org.fusesource.scalate._
import scala.io.Source
import eu.henkelmann.actuarius.ActuariusTransformer
import org.yaml.snakeyaml.Yaml
import java.util.LinkedHashMap
import scala.collection.JavaConversions._
import java.io.{File,FileInputStream,FileOutputStream}

object slurp {
  def apply(file: java.io.File): String = {
    io.Source.fromFile(file.getPath).mkString
  }
}

object writer {
  def apply(filename: String, contents: String) = {
    scala.tools.nsc.io.File(filename).writeAll(contents)
  }
}

object copy {
  def apply(source: String, dest: String) = {
    new FileOutputStream(new File(dest)) getChannel() transferFrom(
        new FileInputStream(new File(source)) getChannel, 0, Long.MaxValue )
  }
}

object makeDirs { def apply(directory: String) = { new File(directory).mkdirs(); } }

class Post(file: java.io.File) {
  def apply(transformer: transformer): String = {
    val scalate = new TemplateEngine
    scalate.bindings = model.map{ case (k,v) => Binding(k, v.getClass.getName) }.toList ::: scalate.bindings

    // extract the template to use from the model
    val template = model.get("template") match {
      case Some(file) => file
      case _ => "default.ssp"
    }

    // get the template file and layout
    val templateFile = transformer.getTemplates.filter( _.getName == template ).head
    scalate.layout(templateFile.getPath, model)
  }

  lazy val contents: String = slurp(file)

  // get the post body contents
  lazy val body: String = {
    val index = headerIndex
    if(index != -1)
      contents.substring(index + 3)
    else
      contents
  }

  // get the yaml header section
  lazy val header: String = {
    val index = headerIndex
    if(index != -1)
      contents.take(index)
    else
      ""
  }

  lazy val headerIndex = { contents.indexOf("---") }

  // make the model that will be used to render this post
  lazy val model: Map[String, Any] = {
    val loadedYaml = (new Yaml().load(header)).asInstanceOf[LinkedHashMap[String,Any]]
    val properties: scala.collection.mutable.Map[String, Any] = if(loadedYaml != null) loadedYaml else new LinkedHashMap[String, Any]()
    Map( "content" -> new ActuariusTransformer()(body)) ++ properties
  }
}

// methods to find all the posts and templates that are needed
class transformer(sourceDir: String, outputDir: String) {
  def mapFilesInDir(file: java.io.File): Map[String, java.io.File] = { file.listFiles.map(f => f.getName -> f).toMap }

  lazy val specialDirectories =
    new File(sourceDir).listFiles.filter(_.isDirectory).filter(_.getName.startsWith("_")).map(t => t.getName -> t).toMap

  def getPosts(): List[java.io.File] = { recursiveListFiles(specialDirectories.get("_posts").get).toList }
  def getTemplates(): List[java.io.File] = { recursiveListFiles(specialDirectories.get("_templates").get).toList }

  def recursiveListFiles(dir: java.io.File): Array[java.io.File] = {
    val dirs = dir.listFiles.filter(_.isDirectory).filter(!_.getName.startsWith("_"))
    val files = dir.listFiles.filter(!_.isDirectory)
    files ++ dirs.flatMap(recursiveListFiles)
  }

  def getOutputPath(file: File): String = {
    (outputDir + file.getPath.substring(sourceDir.length, file.getPath.length)) replace ("_posts/", "")
  }

  def getOutputDir(file: File): String = {
    val fullPath = getOutputPath(file)
    fullPath.substring(0, fullPath.length - file.getName.length)
  }
}

object Main { 
  def main(args: Array[String]) = {
    val sitepath = if(args.size > 0) args(0) else "testsite"
    val output = if(args.size > 1) args(1) else "output"

    val t = new transformer(sitepath, output)
    val staticFiles = t.recursiveListFiles(new File(sitepath)).toList

    // render all of the posts
    t.getPosts().foreach {
      f =>
        val post = new Post(f)(t)
        val outputPath = t.getOutputPath(f)
        val outputDir = t.getOutputDir(f)
        val newpath = outputPath.replace(".md",".html")

        makeDirs(outputDir)

        // render post
        writer(newpath, post)
        println("Transformed: " + newpath)
    }

    // copy other files as is to destination
    staticFiles.foreach {
      f =>
        val outputPath = t.getOutputPath(f)
        val outputDir = t.getOutputDir(f)

        makeDirs(outputDir)

        copy(f.getPath, outputPath)
        println("Copied: " + outputPath)
    }
  }
}

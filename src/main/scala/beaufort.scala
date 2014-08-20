import java.io.File
import org.fusesource.scalate._
import scala.io.Source
import eu.henkelmann.actuarius.ActuariusTransformer
import org.yaml.snakeyaml.Yaml
import java.util.LinkedHashMap
import scala.collection.JavaConversions._

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

class Post(file: java.io.File) {
  def apply(transformer: transformer): String = {
    val scalate = new TemplateEngine
    scalate.bindings = model.map{ case (k,v) => Binding(k, v.getClass.getName) }.toList ::: scalate.bindings
    scalate.layout(transformer.getTemplates.head.getPath, model)
  }

  lazy val contents: String = slurp(file)

  lazy val body: String = {
    val index = headerIndex
    if(index != -1)
      contents.substring(index + 3)
    else
      contents
  }

  lazy val header: String = {
    val index = headerIndex
    if(index != -1)
      contents.take(index)
    else
      ""
  }

  lazy val headerIndex = { contents.indexOf("---") }

  lazy val model: Map[String, Any] = {
    val loadedYaml = (new Yaml().load(header)).asInstanceOf[LinkedHashMap[String,Any]]
    val properties: scala.collection.mutable.Map[String, Any] = if(loadedYaml != null) loadedYaml else new LinkedHashMap[String, Any]()
    Map( "content" -> new ActuariusTransformer()(body)) ++ properties
  }
}

class transformer(dir: String) {
  def mapFilesInDir(file: java.io.File): Map[String, java.io.File] = { file.listFiles.map(f => f.getName -> f).toMap }

  lazy val specialDirectories =
    new File(dir).listFiles.filter(_.isDirectory).filter(_.getName.startsWith("_")).map(t => t.getName -> t).toMap

  def getPosts(): List[java.io.File] = { recursiveListFiles(specialDirectories.get("_posts").get).toList }
  def getTemplates(): List[java.io.File] = { recursiveListFiles(specialDirectories.get("_templates").get).toList }

  def recursiveListFiles(dir: java.io.File): Array[java.io.File] = {
    val dirs = dir.listFiles.filter(_.isDirectory).filter(!_.getName.startsWith("_"))
    val files = dir.listFiles.filter(!_.isDirectory)
    files ++ dirs.flatMap(recursiveListFiles)
  }
}

object Main { 
  def main(args: Array[String]) = {
    val sitepath = if(args.size > 0) args(0) else "testsite"
    val output = if(args.size > 1) args(1) else "output"

    val t = new transformer(sitepath)

    println("Posts: " + t.getPosts)
    println("Templates: " + t.getTemplates)
    println("Files to copy: " + t.recursiveListFiles(new File("testsite")).toList)

    t.getPosts().foreach {
      f => val post = new Post(f)(t)
      val path = output + f.getPath.substring(sitepath.length + 7, f.getPath.length)
      val dir = path.substring(0, path.length - f.getName.length)
      val newpath = path.replace(".md",".html")

      // make directories
      new File(dir).mkdirs();

      // template post
      writer(newpath, post)
      println("Wrote: " + newpath)
    }
  }
}

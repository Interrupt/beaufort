import java.io.File
import org.fusesource.scalate._
import scala.io.Source
import eu.henkelmann.actuarius.ActuariusTransformer

object slurp {
  def apply(file: java.io.File): String = {
    io.Source.fromFile(file.getPath).mkString
  }
}


class Post(file: java.io.File) {
  def apply(transformer: transformer): String = {
    val scalate = new TemplateEngine
    scalate.bindings = model.map{ case (k,v) => Binding(k, v.getClass.getName) }.toList ::: scalate.bindings
    scalate.layout(transformer.getTemplates.head.getPath, model)
  }

  lazy val model: Map[String, Any] = {
    Map( "content" -> new ActuariusTransformer()(slurp(file)))
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
    val t = new transformer(sitepath)

    println("Posts: " + t.getPosts)
    println("Templates: " + t.getTemplates)
    println("Files to copy: " + t.recursiveListFiles(new File("testsite")).toList)

    t.getPosts().foreach { f => new Post(f)(t) }
  }
}

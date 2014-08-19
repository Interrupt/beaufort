import java.io.File
import org.fusesource.scalate._
import scala.io.Source
import eu.henkelmann.actuarius.ActuariusTransformer

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

  def render(file: java.io.File, model: Map[String, Any]): String = {
    val scalate = new TemplateEngine
    scalate.bindings = model.map{ case (k,v) => Binding(k, v.getClass.getName) }.toList ::: scalate.bindings
    val source = TemplateSource.fromFile(file.getPath)
    scalate.layout(source, model)
  }

  def getModelFromPost(post: java.io.File): Map[String, Any] = {
    Map( "content" -> new ActuariusTransformer()(readFile(post)))
  }

  def renderPost(post: java.io.File, template: java.io.File): String = {
    render(template, getModelFromPost(post))
  }

  def readFile(file: java.io.File): String = {
    io.Source.fromFile(file.getPath).mkString
  }
}

object Main { 
  def main(args: Array[String]) = {
    val t = new transformer("testsite")
    println("Posts: " + t.getPosts)
    println("Templates: " + t.getTemplates)
    println("Files to copy: " + t.recursiveListFiles(new File("testsite")).toList)
    println(t.renderPost(t.getPosts.head, t.getTemplates.head))
  }
}

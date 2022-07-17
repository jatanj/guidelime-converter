package dev.jatan.wow.guide

import org.jsoup.nodes.{Element, Node, TextNode}

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Utils {
  def fileExtension(fullName: String) = {
    val name = new File(fullName).getName
    val dotIndex = name.lastIndexOf('.')
    if (dotIndex == -1) "" else name.substring(dotIndex + 1)
  }

  implicit class StringExtension(val s: String) extends AnyVal {
    def trimLeft = s.replaceAll("^\\s+", "")
  }

  implicit class StringBuilderExtension(val builder: StringBuilder) extends AnyVal {
    def appendLine(s: String) = {
      builder.append(s)
      builder.append('\n')
    }
  }

  implicit class ElementExtension(val element: Element) extends AnyVal {
    def allChildNodes(filter: Node => Boolean = _ => true): mutable.Buffer[Node] = {
      val stack = mutable.Stack.from(element.childNodes().asScala)
      val visited = mutable.Set.empty[Node]
      val result = mutable.Buffer.empty[Node]
      while (stack.nonEmpty) {
        stack.pop() match {
          case node: TextNode => result += node
          case node =>
            if (visited.contains(node)) {
              result += node
            } else {
              if (filter(node)) {
                stack.push(node)
                val childNodes = node.childNodes()
                if (!childNodes.isEmpty) {
                  for (i <- childNodes.size - 1 to 0 by -1) {
                    stack.push(childNodes.get(i))
                  }
                }
              }
              visited += node
            }
        }
      }
      result
    }
  }
}

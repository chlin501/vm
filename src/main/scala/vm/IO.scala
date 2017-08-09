package vm

import scala.io.Source	
import java.nio.file.Paths

object IO {

  def from(path: String): Iterable[String] = {
    require(null != path && !path.isEmpty, "Empty file path!")
    Source.fromFile(Paths.get(path).toFile).getLines.toIterable
  }

}


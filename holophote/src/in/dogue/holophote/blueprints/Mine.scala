package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities._

case class Mine(k:Int, r:(Int,Int,Int,Int)) {
  self=>

  def generate(g:Graph[Vox,Vox]):(Map[Job, List[Goal]],Map[Job, List[Goal]]) = {
    val x = r._1
    val y = r._2
    val width = r._3
    val height = r._4
    val center = (width/2, height/2)
    val cmds =(for (i <- 0 until width;
                    j <- 0 until height if (i,j) != center) yield {
      (i, j)
    }).sortBy { case p =>
      (p |-| center).mag2
    }.map { case (i, j) =>
      Dig.create((i+x, j+y, k), Tunnel)
    }
    val stPos = (x + width/2, y + height/2, k+1)
    val major = Map[Job, List[Goal]](Miner -> (List(Dig.create(stPos, StairDown(stPos |-|-| (0,0,1)))) ++ cmds))
    val minor = Blueprint.mkMap
    major @@ minor
  }
  def toBlueprint = new Blueprint {
    def generate(g:Graph[Vox,Vox]):(Map[Job, List[Goal]],Map[Job, List[Goal]]) = self.generate(g)
  }
}

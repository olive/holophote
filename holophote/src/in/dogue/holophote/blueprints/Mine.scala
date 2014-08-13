package in.dogue.holophote.blueprints

import in.dogue.antiqua.data.Graph
import in.dogue.antiqua.Antiqua._
import in.dogue.holophote.entities._
import in.dogue.holophote.Schema

case class Mine(k:Int, r:(Int,Int,Int,Int)) {
  self=>

  def generate(g:Graph[Vox,Vox]):(Seq[(Job, PlanId=>Goal)], Seq[(Job, PlanId=>Goal)]) = {
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
      Miner -> Dig.create((i+x, j+y, k), Tunnel) _
    }
    val stPos = (x + width/2, y + height/2, k+1)
    val major = List(Miner -> Dig.create(stPos, StairDown(stPos |-|-| (0,0,1))) _) ++ cmds
    val minor = List()
    major @@ minor
  }
  def toBlueprint = new Blueprint {
    def generate(g:Graph[Vox,Vox]) = self.generate(g)
  }
}

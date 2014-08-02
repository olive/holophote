package in.dogue.holophote.entities

case class EntityAction(f:Entity => Entity, tMax:Int, t:Int) {
  def update = copy(t=t+1)
  def process(e:Entity):Entity = f(e)
  def isDone:Boolean = t >= tMax
}

class EntityScript(map:Map[Int,EntityAction]) {

}

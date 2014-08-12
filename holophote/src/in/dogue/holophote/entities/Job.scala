package in.dogue.holophote.entities

sealed trait Job
case object Builder extends Job
case object Gatherer extends Job
case object Miner extends Job
case object Supervisor extends Job

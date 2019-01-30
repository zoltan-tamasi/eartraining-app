package net.zoltantamasi.eartraining.state

import net.zoltantamasi.eartraining.Chord

sealed trait Effect
case object NoEffect extends Effect
case class PlayChordEffect(chord: Chord) extends Effect

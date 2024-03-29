package net.zoltantamasi.eartraining

import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{AudioBuffer, AudioContext, Event}
import org.scalajs.dom.{console, window}

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Failure, Success}

trait AudioEngine {
  def playChord(chord: Chord): Unit
}

sealed trait AudioEvent
case object AudioFinished extends AudioEvent

object AudioEngine {

  implicit val executor: ExecutionContext = scala.concurrent.ExecutionContext.global

  //twelfth root of 2
  val successorMultiplier = 1.05946309436
  //reciprocal of above
  val predecessorMultiplier = 0.94387431268

  private def playSound(note: Note)(implicit context: AudioContext, audioBuffers: Map[Note, (AudioBuffer, Double)]): Future[Unit] = {
    val source = context.createBufferSource()
    val audioBuffer = audioBuffers.get(note)
    val gain = context.createGain()

    val p = Promise[Unit]()

    source.buffer = audioBuffer match {
      case Some((audioBuffer, _)) => audioBuffer
      case None => {
        println(s"Not supported note: $note")
        audioBuffers.get(note.copy(octave = 5)).get._1
      }
    }

    source.connect(gain)
    gain.connect(context.destination)
    source.playbackRate.value = audioBuffer.get._2
    source.onended = (event: Event) => {
      p.success(())
    }

    window.setTimeout(() => {
      p.tryFailure(new Throwable("Future timed out"))
    }, 4000)

    gain.gain.linearRampToValueAtTime(0, context.currentTime + 2)
    source.start(0)
    p.future
  }

  def createWithAudioContext(context: AudioContext, handler: (AudioEvent) => Unit): Future[AudioEngine] = {

    def loadSound(url: String): Future[AudioBuffer] = {
      val p = Promise[AudioBuffer]()
      Ajax
        .get("sounds/" + url, responseType = "arraybuffer")
        .map(request => {
          val arrayBuffer = request.response.asInstanceOf[ArrayBuffer]
          context.decodeAudioData(arrayBuffer, (buffer: AudioBuffer) => {
            p.success(buffer)
          }, () => {
            p.failure(new Throwable(s"Unable to load sound from: ${url}"))
          })
        })
      p.future
    }

    def getFilledAudioBufferMap(audioBuffers: Map[Note, (AudioBuffer, Double)]): Map[Note, (AudioBuffer, Double)] = {

      @tailrec
      def fillStep(audioBuffers: Map[Note, (AudioBuffer, Double)]): Map[Note, (AudioBuffer, Double)] = {
        var foundGap = false
        audioBuffers.keys.foreach(note => {
          if (note >= Note(C, 2) && note <= Note(B, 6)) {
            if (!audioBuffers.isDefinedAt(note++)) {
              foundGap = true
              audioBuffers.put(note++, (audioBuffers.get(note).get._1, audioBuffers.get(note).get._2 * successorMultiplier))
            }
            if (!audioBuffers.isDefinedAt(note--)) {
              foundGap = true
              audioBuffers.put(note--, (audioBuffers.get(note).get._1, audioBuffers.get(note).get._2 * predecessorMultiplier))
            }
          }
        })
        println(audioBuffers.toList.sortWith { case ((noteA, _), (noteB, _)) => noteA <= noteB })
        if (foundGap) {
          fillStep(audioBuffers)
        } else {
          audioBuffers
        }
      }

      fillStep(audioBuffers)
    }

    for {
      a1 <- loadSound("a1mmell.wav")
      a2 <- loadSound("a2mmell.wav")
      a3 <- loadSound("a3mmell.wav")
      a4 <- loadSound("a4mmell.wav")
      a5 <- loadSound("a5mmell.wav")
      a6 <- loadSound("a6mmell.wav")
      a7 <- loadSound("a7mmell.wav")

      c2 <- loadSound("c2mmell.wav")
      c3 <- loadSound("c3mmell.wav")
      c4 <- loadSound("c4mmell.wav")
      c5 <- loadSound("c5mmell.wav")
      c6 <- loadSound("c6mmell.wav")
      c7 <- loadSound("c7mmell.wav")

      eb2 <- loadSound("eb2mmell.wav")
      eb3 <- loadSound("eb3mmell.wav")
      eb4 <- loadSound("eb4mmell.wav")
      eb5 <- loadSound("eb5mmell.wav")
      eb6 <- loadSound("eb6mmell.wav")
      eb7 <- loadSound("eb7mmell.wav")

      fs1 <- loadSound("f%231mmell.wav")
      fs2 <- loadSound("f%232mmell.wav")
      fs3 <- loadSound("f%233mmell.wav")
      //fs4 <- loadSound("f%234mmell.wav")
      fs5 <- loadSound("f%235mmell.wav")
      fs6 <- loadSound("f%236mmell.wav")
      fs7 <- loadSound("f%237mmell.wav")

    } yield {

      implicit val audioBuffers = getFilledAudioBufferMap(Map(

        Note(A, 1) -> (a1, 1.0),
        Note(A, 2) -> (a2, 1.0),
        Note(A, 3) -> (a3, 1.0),
        Note(A, 4) -> (a4, 1.0),
        Note(A, 5) -> (a5, 1.0),
        Note(A, 6) -> (a6, 1.0),
        Note(A, 7) -> (a7, 1.0),

        Note(C, 2) -> (c2, 1.0),
        Note(C, 3) -> (c3, 1.0),
        Note(C, 4) -> (c4, 1.0),
        Note(C, 5) -> (c5, 1.0),
        Note(C, 6) -> (c6, 1.0),
        Note(C, 7) -> (c7, 1.0),

        Note(D_#, 2) -> (eb2, 1.0),
        Note(D_#, 3) -> (eb3, 1.0),
        Note(D_#, 4) -> (eb4, 1.0),
        Note(D_#, 5) -> (eb5, 1.0),
        Note(D_#, 6) -> (eb6, 1.0),
        Note(D_#, 7) -> (eb7, 1.0),

        Note(F_#, 1) -> (fs1, 1.0),
        Note(F_#, 2) -> (fs2, 1.0),
        Note(F_#, 3) -> (fs3, 1.0),
        //Note(F_#, 4) -> (fs4, 1.0),
        Note(F_#, 5) -> (fs5, 1.0),
        Note(F_#, 6) -> (fs6, 1.0),
        Note(F_#, 7) -> (fs7, 1.0)
      ))

      implicit val audioContext = context

      new AudioEngine {
        override def playChord(chord: Chord): Unit = {
          println(s"Chord played: ${Chord.notesOf(chord)}")
          Future.sequence(Chord.notesOf(chord).map(playSound _))
            .onComplete({
              case Success(_) =>
                handler(AudioFinished)
              case Failure(throwable) =>
                console.error(throwable.toString)
            })
        }
      }
    }
  }
}
import forcomp.Anagrams.{Occurrences, Sentence, Word}
import forcomp.loadDictionary

def wordOccurrences(w: Word)= (w groupBy(x => x) mapValues(_.length) toList) sorted
/*
def wordOccurrences(w: Word) = {
  def wordAcc(acc: Map[Char,Int], c: Char) = {
    val cnt = (acc get c).getOrElse(0) + 1
    acc + ((c, cnt))
  }
  ((w toLowerCase)  foldLeft Map[Char,Int]())(wordAcc) toList
}
*/

def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences((s foldLeft "")(_ + _))

sentenceOccurrences(List("qwe", "wer", "wer"))

val dictionary: List[Word] = loadDictionary

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary groupBy(x => wordOccurrences(x))

dictionaryByOccurrences.values.toList

def wordAnagrams(word: Word): List[Word] = (for(value <- dictionaryByOccurrences.values.toList; if (value.contains(word))) yield value).flatten

wordAnagrams("ate")

val data:Occurrences =  List(('a', 2), ('b', 2), ('c',1))


def combinations(occurrences: Occurrences): List[Occurrences] = {

  def ascCombi(idx:Int, coeff:Int):List[Occurrences] = {
    val (c,n) = occurrences(idx)
    if(coeff > n) List()
    else if (coeff == 0) List() :: ascCombi(idx, coeff+1)
    else List((c,coeff)) :: ascCombi(idx, coeff+1)
  }

  val l = occurrences.length

  if(l<1) List(Nil)
  else (((1 until l) map (x => ascCombi(x,0))) foldLeft ascCombi(0,0))((x,y) => for(a<-x;b<-y) yield a++b)

}
combinations(data)

val q = Vector(List(List(), List(('a',1)), List(('a',2))), List(List(), List(('b',1)), List(('b',2))))
val qq = List(List(), List(('a',1)), List(('a',2)))
val ww = List(List(), List(('b',1)), List(('b',2)))
val ee = List(List())


//qq.foldLeft(ww)((x,y)=>x.foldLeft(y)(_:::_))

//val w = (q foldLeft List[Occurrences]())((x,y) => (a<-x) )

/*
def combi(idx:Int): Occurrences = {
  if(idx < 0) List()
  if(coeff > 0) combi((idx-1) :+ data(idx)
  else combi(idx-1, )
}
combi(2,1)
*/

val rr = List(('a',2), ('b',5), ('c',1))
val rq = List(('a',1), ('b',4), ('d',2))
val re = List(('a',1), ('b',2))

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  (y.toMap foldLeft x.toMap)((a,b) =>
    a.get(b._1) match {
      case None => a
      case Some(i) =>
        if (i > b._2) a.updated(b._1, i-b._2)
        else a-b._1
    }
  ).toList
}

subtract(rr,rq)

def contain(x: Occurrences, y: Occurrences):Boolean = subtract(y,x).isEmpty

contain(rr,rq)
contain(rr,re)


def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

  def pickWord(occur: Occurrences):List[Sentence] = {
    for(occ <- combinations(occur); word <- dictionaryByOccurrences get occ) yield word
  }

  val occ = sentenceOccurrences(sentence)

  for {
    o <- combinations(occ)
    o1 <- combinations(subtract(occ,o))
    o2 <- combinations(subtract(subtract(occ,o),o1))

    w <- pickWord(o)
    w1 <- pickWord(o1)
    w2 <- pickWord(o2)

    if(!w.isEmpty && !w1.isEmpty && !w2.isEmpty)
  } yield w++w1++w2

}

sentenceAnagrams(List("Yes", "man"))


for {
  occ <- combinations(sentenceOccurrences(List("Yes", "man")))
  word <- dictionaryByOccurrences get occ
}yield word




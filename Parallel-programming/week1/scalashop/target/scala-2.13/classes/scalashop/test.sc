val height = 1920
val task = 32

def subTask(length: Int, task: Int): Array[(Int, Int)] = {
  val step = if (length % task == 0) length / task else length / task + 1
  val sublist = 0 to length by step

  var res = sublist.map(_ + 1).zip(sublist.tail).toArray

  if (length % task != 0) {
    res = res :+ (res.last._2, length)
  }

  res
}

subTask(height, task)
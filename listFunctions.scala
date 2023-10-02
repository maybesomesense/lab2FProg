object Main234 {
  def main(args: Array[String]): Unit = {
    print("VARIANT FIRST\n" +
          "1 — first task \n" +
          "2 — second task\n" +
          "3 — third task\n" +
          "4 — fourth task\n" +
          "5 — fifth task\n" +
          "6 — generator" +
          "Enter task: ")

    val choice = scala.io.StdIn.readInt()

    if(choice == 1){
      print("Enter size of list: ")
      val listSize = scala.io.StdIn.readInt()
      val list = defineList(listSize)
      print(sumOfListElements(list))
      //val sum = list.reduce((x, y) => x + y)
      //print(sum)
    } else if(choice == 2){
      val list = defineList(10)
      print(SumOfFirstThreeElements(list, 0))
    } else if(choice == 3){
      print("Enter size of list: ")
      val listSize = scala.io.StdIn.readInt()
      val list = defineList(listSize)
      print(searchIndexOfMaxElement(list, -1, 0, 0))
    } else if(choice == 4){
      print("Enter size of list: ")
      val listSize = scala.io.StdIn.readInt()
      val list = defineList(listSize)
      if(list.nonEmpty) {
        if (checkExchange(list.tail, list.head)) { print("Ordered list\n") }
        else {print("Unordered list\n")}
      } else{
        print("Empty list\n")
      }
    } else if(choice == 5){
      print("Enter size of list: ")
      val listSize = scala.io.StdIn.readInt()
      val list = defineList(listSize)

      val element = list.head
      val partOfList = list.tail
      if(checkUniqueElements(list, partOfList, element)){
        print("All elements are unique\n")
      } else{
        print("Not all elements are unique\n")
      }
    } else if(choice == 6){
      generator()
    }

  }

  // первая задача
  def sumOfListElements(list: List[Int]): Int = {
    if(list.nonEmpty) {
      val element = list.head
      var partOfList = list.tail

      element + sumOfListElements(partOfList)
    } else {
      0
    }
  }

  // вторая задача
  def SumOfFirstThreeElements(list: List[Int], attempt: Int): Int = {
    if(list.nonEmpty && attempt != 3) {
      val element = list.head
      var partOfList = list.tail

      val newAttempt = attempt + 1
      element + SumOfFirstThreeElements(partOfList, newAttempt)
    } else {
      0
    }
  }


  def defineList(listSize: Int): List[Int] = {
    var list = List.empty[Int]
    for(i <- 1 to listSize){
      print(s"Enter element $i: ")
      val element = scala.io.StdIn.readInt()
      list = list :+ element
    }

    list
  }

  // третья задача
  def searchIndexOfMaxElement(list: List[Int], maxElement: Int, index: Int, itteration: Int): Int ={
    if(list.nonEmpty) {
      val element = list.head
      val partOfList = list.tail

      if(maxElement < element){
        searchIndexOfMaxElement(partOfList, element, itteration + 1, itteration + 1)
      } else{
        searchIndexOfMaxElement(partOfList, element, index, itteration + 1)
      }
    } else {
      index
    }
  }

  // четвёртая задача
  def checkExchange(list: List[Int], leftElement: Int): Boolean ={
    if(list.nonEmpty) {
      val rightElement = list.head
      val partOfList = list.tail

      if(leftElement < rightElement){
        checkExchange(partOfList, rightElement)
      } else{
        false
      }
    } else {
      true
    }
  }

  // пятая задача
  def checkUniqueElements(list: List[Int], partOfList: List[Int], currentElement: Int): Boolean ={
    if(list.nonEmpty){
      if(partOfList.nonEmpty){
        if(currentElement == partOfList.head){
          return false
        }
        checkUniqueElements(list, partOfList.tail, currentElement)
      } else{
        checkUniqueElements(list.tail, list.tail, list.head)
      }
    } else{
     true
    }
  }

  def generator(): Unit = {
    print("Input count: ");
    var count: Int = scala.io.StdIn.readInt();
    var part: Double = 100.0 / count;
    var list = List.empty[Double];

    def generate(start: Double = 0.0): Unit = {
      var end: Double = start + part;
      if(end > 100.0) return;
      var number: Double = Math.sqrt(start * end);
      if(start == 0.0){
        number = Math.sqrt(end);
      }
      list = list :+ number;
      println(s"${number}   ( ${start} , ${end} )");
      generate(start + part);
    }

    generate();
  }
}
/* -------------------------------------------------------------------------- 

   Complete code for the command-line interpreter


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/

class Expr
case class bacht_ast_empty_agent() extends Expr
case class bacht_ast_primitive(primitive: String, token: String) extends Expr
case class bacht_ast_primitive_with_time(primitive: String, token: String,time: Int) extends Expr
case class bacht_ast_dead_agent() extends Expr
case class bacht_ast_delay(token: Int) extends Expr
case class bacht_ast_agent(op: String, agenti: Expr, agentii: Expr) extends Expr
import scala.util.parsing.combinator._
import scala.util.matching.Regex

class BachTParsers extends RegexParsers {

  def token 	: Parser[String] = ("[a-z][0-9a-zA-Z_]*").r ^^ {_.toString}
  def time 	: Parser[String] = ("[0-9]*").r ^^ {_.toString}

  val opChoice  : Parser[String] = "+" 
  val opPara    : Parser[String] = "||"
  val opSeq     : Parser[String] = ";" 
 
  def primitive : Parser[Expr]   = "tell("~token~"," ~time~")" ^^ {
        case _ ~ vtoken ~ "," ~ vtime ~ _ => bacht_ast_primitive_with_time("tell",vtoken,vtime.toInt) }  |
                                   "ask("~token~"," ~time~")" ^^ {
        case _ ~ vtoken ~ "," ~ vtime ~ _  => bacht_ast_primitive_with_time("ask",vtoken,vtime.toInt) }   |
                                   "get("~token~"," ~time~")" ^^ {
        case _ ~ vtoken ~ "," ~ vtime ~ _  => bacht_ast_primitive_with_time("get",vtoken,vtime.toInt) }   |
                                   "nask("~token~"," ~time~")" ^^ {
        case _ ~ vtoken ~ "," ~ vtime ~ _  => bacht_ast_primitive_with_time("nask",vtoken,vtime.toInt) }

  def agent = compositionChoice

  def compositionChoice : Parser[Expr] = compositionPara~rep(opChoice~compositionChoice) ^^ {
        case ag ~ List() => ag
        case agi ~ List(op~agii)  => bacht_ast_agent(op,agi,agii) }

  def compositionPara : Parser[Expr] = compositionSeq~rep(opPara~compositionPara) ^^ {
        case ag ~ List() => ag
        case agi ~ List(op~agii)  => bacht_ast_agent(op,agi,agii) }

  def compositionSeq : Parser[Expr] = simpleAgent~rep(opSeq~compositionSeq) ^^ {
        case ag ~ List() => ag
        case agi ~ List(op~agii)  => bacht_ast_agent(op,agi,agii) }

  def simpleAgent : Parser[Expr] = primitive | parenthesizedAgent

  def parenthesizedAgent : Parser[Expr] = "("~>agent<~")"

}

object BachTSimulParser extends BachTParsers {

  def parse_primitive(prim: String) = parseAll(primitive,prim) match {
        case Success(result, _) => result
        case failure : NoSuccess => scala.sys.error(failure.msg)
  }

  def parse_agent(ag: String) = parseAll(agent,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => scala.sys.error(failure.msg)
  }

}
import scala.collection.mutable.Map
import scala.swing._

class BachTStore {

   var theStoreTime = Map[(String,Int),Int]()
   var theStore = Map[String,Int]()
   var timeStore:Int = 0

   def tell(token:String):Boolean = {
      if (theStore.contains(token)) 
        { theStore(token) = theStore(token) + 1 }
      else
        { theStore = theStore ++ Map(token -> 1) }
      true
   }


   def ask(token:String):Boolean = {
      if (theStore.contains(token)) 
             if (theStore(token) >= 1) { true }
             else { false }
      else false
   }


   def get(token:String):Boolean = {
      if (theStore.contains(token)) 
             if (theStore(token) >= 1) 
               { theStore(token) = theStore(token) - 1 
                 true 
               }
             else { false }
      else false
   }


   def nask(token:String):Boolean = {
      if (theStore.contains(token)) 
             if (theStore(token) >= 1) { false }
             else { true }
      else true 
   }

  // Version avec time
  def tell_time(token:String,time:Int):Boolean = {
    val tokenTime = (token,time+timeStore)
    if (theStoreTime.contains(tokenTime))
    { theStoreTime(tokenTime) = theStoreTime(tokenTime) + 1 }
    else
    { theStoreTime = theStoreTime ++ Map(tokenTime -> 1) }
    true
  }


  def ask_time(token:String,time:Int):Boolean = {
    val listGoodToken =theStoreTime.filter(x => (x._1._1.equals(token))&&(x._1._2-timeStore>0)&&(x._2>=1))
    println("askTime")
    println(listGoodToken)
    if (listGoodToken.nonEmpty){true}
    else false

  }

  //todo
  def get_time(token:String,time:Int):Boolean = {
    val listGoodToken =theStoreTime.filter(x => (x._1._1.equals(token))&&(x._1._2-timeStore>0)&&(x._2>=1))
    if(listGoodToken.nonEmpty){
      var acc = listGoodToken.last
      val tokenGet =listGoodToken.foldLeft(acc){(acc, token)=> if(acc._1._2<=token._1._2){acc}else{token}}
      theStoreTime(tokenGet._1)=theStoreTime(tokenGet._1)-1
      true
    }
    else false

  }


  def nask_time(token:String,time:Int):Boolean = {
    val listGoodToken =theStoreTime.filter(x => (x._1._1.equals(token))&&(x._1._2-timeStore>0)&&(x._2>=1))
    println("naskTime")
    println(listGoodToken)
    if (listGoodToken.isEmpty){true}
    else false
  }

  /*
  On avance d'une unité de temps le store et on supprime les tokens qui ne sont plus qui on passé leur temps de validité
   */
  def spent_time()={
    timeStore=timeStore+1
    theStoreTime=theStoreTime.filter(t=>t._1._2>timeStore)
  }

   def print_store {
     println("Time Store : "+timeStore)
      print("{ ")
      for ((t,d) <- theStore) 
         print ( t + "(" + theStore(t) + ")" )
     for ((t,d) <- theStoreTime)
       print ( t + "(" + theStoreTime(t) + ")" )
      println(" }")
   }

   def clear_store {
      theStore = Map[String,Int]()
   }

}

object bb extends BachTStore {

   def reset { clear_store }

}
import scala.util.Random
import language.postfixOps

class BachTSimul(var bb: BachTStore) {

   val bacht_random_choice = new Random()

   def run_one(agent: Expr):(Boolean,Expr) = {

      agent match {
         case bacht_ast_primitive(prim,token) =>
            {  if (exec_primitive(prim,token)) { (true,bacht_ast_empty_agent()) }
               else { (false,agent) }
            }
         case bacht_ast_primitive_with_time(prim,token,time) =>
           {  if (exec_primitive_with_time(prim,token,time)) { (true,bacht_ast_empty_agent()) }
           else { (false,bacht_ast_delay(0)) }
           }

         case bacht_ast_dead_agent()=> (false,agent)

         case bacht_ast_agent(";",ag_i,ag_ii) =>
            {  run_one(ag_i) match
                  { case (false,_) => (false,agent)
                    case (true,bacht_ast_empty_agent()) => (true,ag_ii)
                    case (true,ag_cont) => (true,bacht_ast_agent(";",ag_cont,ag_ii))
                  }
            }

         case bacht_ast_agent("||", ag_i, ag_ii) =>{
           var branch_choice = bacht_random_choice.nextInt(2)

           var x = ag_ii
           var y = ag_i
           if (branch_choice == 0){
             x = ag_i
             y = ag_ii
           }

           run_one( x ) match
             {

             case (false,bacht_ast_delay(time)) =>
               { run_one( y ) match
                 {
                   case (false,_) => (false,bacht_ast_delay(time))
                   case (true,bacht_ast_empty_agent()) => (true,x)
                   case (true,ag_cont) => (true,bacht_ast_agent("||",x,ag_cont))
                 }
               }

              case (false,_) =>
              { run_one( y ) match
               {
                 case (false,bacht_ast_delay(time)) => (false,bacht_ast_delay(time))
                 case (false,_) => (false,agent)
                 case (true,bacht_ast_empty_agent()) => (true,x)
                 case (true,ag_cont) => (true,bacht_ast_agent("||",x,ag_cont))
               }
             }
             case (true,bacht_ast_empty_agent()) => (true,y)

             case (true,ag_cont)=> (true,bacht_ast_agent("||",ag_cont,y))

             }


         }


         case bacht_ast_agent("+",ag_i,ag_ii) =>{
           var branch_choice = bacht_random_choice.nextInt(2)
           var x = ag_ii
           var y = ag_i
           if (branch_choice == 0){
             x = ag_i
             y = ag_ii
           }

           run_one( x ) match
             {
             case (false,bacht_ast_delay(time)) =>
               { run_one( y ) match
                 { case (false,_) => (false,bacht_ast_delay(time))
                   case (true,bacht_ast_empty_agent()) => (true,bacht_ast_empty_agent())
                   case (true,ag_cont) => (true,ag_cont)
                 }
               }

             case (false,_) =>
               { run_one( y ) match
                 {
                   case (false,bacht_ast_delay(time)) => (false,bacht_ast_delay(time))
                   case (false,_) => (false,agent)
                   case (true,bacht_ast_empty_agent()) => (true,bacht_ast_empty_agent())
                   case (true,ag_cont) => (true,ag_cont)
                 }
               }
             case (true,bacht_ast_empty_agent()) => (true,bacht_ast_empty_agent())
             case (true,ag_cont) => (true,ag_cont)
             }


         }
      }

   }

  /*
  Applique le passage d'une unité de temps à agent
   */
  def sleep_time(agent:Expr):Expr = agent match {
    case bacht_ast_primitive(prim,token) => agent

    case bacht_ast_primitive_with_time(prim,token,time)=> {
      if(time>1) {
        bacht_ast_primitive_with_time(prim, token, time - 1)
      }else bacht_ast_dead_agent()
    }



    case bacht_ast_agent(";",ag_i,ag_ii) => bacht_ast_agent(";",sleep_time(ag_i),ag_ii)

    case bacht_ast_agent("||", ag_i, ag_ii) =>bacht_ast_agent("||", sleep_time(ag_i), sleep_time(ag_ii))



    case bacht_ast_agent("+",ag_i,ag_ii) =>bacht_ast_agent("+",sleep_time(ag_i), sleep_time(ag_ii))

  }

   def bacht_exec_all(agent: Expr):Boolean = {

       var failure = false
       var c_agent = agent
       while ( c_agent != bacht_ast_empty_agent() && !failure ) {
          failure = run_one(c_agent) match
               {
                case (false,bacht_ast_delay(time))=> {
                  bb.spent_time()
                  println("on passe un tour "+time)
                  println("c_agent actuel : "+c_agent)
                  c_agent=sleep_time(c_agent)
                  println("c_agent new : "+c_agent)
                  false
                } // gestion du cas ou on ne peut rien faire sauf décrémenter les delays
                 case (false,_)          => true
                 case (true,new_agent)  =>
                    { c_agent = new_agent
                      false
                    }
               }
           bb.print_store
           println("\n")
       }

       if (c_agent == bacht_ast_empty_agent()) {
           println("Success\n")
           true
       }
       else {
           println("failure\n")
           false
       }
   }

   def exec_primitive(prim:String,token:String):Boolean = {
       prim match
         { case "tell" => bb.tell(token)
           case "ask"  => bb.ask(token)
           case "get"  => bb.get(token)
           case "nask" => bb.nask(token)
         }
   }



  def exec_primitive_with_time(prim:String,token:String,time: Int):Boolean = {
    prim match
    { case "tell" => bb.tell_time(token,time)
    case "ask"  => bb.ask_time(token,time)
    case "get"  => bb.get_time(token,time)
    case "nask" => bb.nask_time(token,time)
    }
  }
}

object ag extends BachTSimul(bb) {

  def apply(agent: String) {
     val agent_parsed = BachTSimulParser.parse_agent(agent)
     ag.bacht_exec_all(agent_parsed) 
  }
  def eval(agent:String) { apply(agent) }
  def run(agent:String) { apply(agent) }

}
         

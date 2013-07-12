object tests {

  object t1 {
  	import comparing.s01._
  	
  	val r = sort( List(new Dinheiro(3), new Dinheiro(2), new Dinheiro(1)) )
  	
  	println( r )
 	}
 	
 	object t3 {
  	import comparing.s03._
  	
  	val r = sort( List(new Dinheiro(3), new Dinheiro(2), new Dinheiro(1)) )(comparaDinheiro)
  	
  	println( r )
 	}
 	
 	object t4 {
 		import comparing.TimeLib._
  	import comparing.s04._
  	
  	val r = sort( List(Timestamp.today, Timestamp.yesterday) )(comparaTimestamps)
  	
  	println( r )
 	}
 	
 	t3                                        //> List(Dinheiro(1), Dinheiro(2), Dinheiro(3))
                                                  //| res0: tests.t3.type = tests$t3$@3cf720d5
 	t4                                        //> List(Timestamp(0), Timestamp(-1))
                                                  //| res1: tests.t4.type = tests$t4$@a540251
 
 
}
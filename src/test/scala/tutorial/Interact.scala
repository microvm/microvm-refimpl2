package tutorial

import uvm.refimpl._

object Interact extends App {
  
  // Create the Mu instance
  val microVM = new MicroVM()     // #1
  
  // Implicitly convert names to IDs
  implicit def idOf(name: String) = microVM.idOf(name)  // #2

  // Create the context
  val ctx = microVM.newContext()  // #3

  ctx.loadBundle("""              // #4
.typedef @i64 = int<64>

.const @I64_1 <@i64> = 1

.funcsig @main.sig = (@i64) -> ()

.funcdef @main VERSION %v1 <@main.sig> {    // #12
  %entry(<@i64> %n):
    %n2 = ADD <@i64> %n @I64_1              // #13
    [%trap] TRAP <> KEEPALIVE (%n2)         // #14
    COMMINST @uvm.thread_exit               // #21
}
""")
  
  // Create the trap handler
  val myTrapHandler = new TrapHandler {
    def handleTrap(ctx: MuCtx, thread: MuThreadRefValue,
        stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
      
      // Create a cursor to introspect the stack
      val cursor = ctx.newCursor(stack)        // #15
      val curInstID = ctx.curInst(cursor)      // #16
      
      ctx.nameOf(curInstID) match {
        case "@main.v1.entry.trap" => {        // #17
          // Dump the keep-alive variables
          val Seq(n2: MuIntValue) = ctx.dumpKeepalives(cursor)    // #18
          ctx.closeCursor(cursor)
          
          // Print the value
          val n2Int = ctx.handleToSInt(n2)            // #19
          printf("The value of n2 is %d.\n", n2Int)
          
          // Return to Mu from the trap handler
          TrapHandlerResult.Rebind(stack, HowToResume.PassValues(Seq()))  // #20
        }
      }
    }
  }
  
  // Set the trap handler
  microVM.setTrapHandler(myTrapHandler)           // #5

  // Create the stack and the thread
  val mainFunc = ctx.handleFromFunc("@main")      // #6
  val st = ctx.newStack(mainFunc)                 // #7
  
  val fortyTwo = ctx.handleFromInt(42, 64)        // #8
  
  val th = ctx.newThread(st, None, HowToResume.PassValues(Seq(fortyTwo)))  // #9
  
  // Close the context
  ctx.closeContext()    // #10
  
  // Let the reference implementation run
  microVM.execute()     // #11
  
  // #22
}

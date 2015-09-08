package uvm.clientsupport.text

import java.util._

import scala.beans._

import DefaultTypes._

/**
 * NameFactory creates non-duplicated names.
 * <p>
 * This tool uses the scheme @prefix_num or @prefix_num_hint. In this way, as long as two NameFactory instances
 * have two different prefixes and one is not a prefix of the other, they are guaranteed to give a unique name
 * each time nextName is called. The "hint" argument provides a human-readable suffix to a name.
 * <p>
 * It is not recommended to use local names in generated code, since they
 * strictly aliases of @funcvername.localname.
 * <p>
 * Example:
 * <pre>
 * val instNF = new NameFactory("@inst")
 * val name1 = instNF.nextName()      // @inst_0000
 * val name2 = instNf.nextName("add") // @inst_0001_add
 * </pre>
 */
class NameFactory(prefix: String) {
  private var num = 0
  
  def nextName(): MuName = nextName(null)

  def nextName(hint: String): MuName = {
    val myNum = num
    num += 1
    if (hint == null) {
      "%s_%04d".format(prefix, myNum)
    } else {
      "%s_%04d_%s".format(prefix, myNum, hint)
    }
  }
}

/**
 * A helper class for building function definitions.
 * <p>
 * An entry block is
 */
class FuncDefBuilder(name: MuName, version: MuName, sig: MuName) {
  @BeanProperty
  val funcDef = new FuncDef()
  funcDef.name = name
  funcDef.version = version
  funcDef.sig = sig

  // Automatic name generation
  private val paramNamer = new NameFactory(version + ".param")
  private val bbNamer = new NameFactory(version + ".bb")
  @BeanProperty
  val instNamer = new NameFactory(version + ".inst")
  
  // Some convenient functions because the client may frequently request new names.
  def nextInstName(hint: String): String = instNamer.nextName(hint)
  def nextInstName(): String = instNamer.nextName()

  /**
   * Add a parameter.
   *
   * @param hint A human-readable suffix of the parameter, or null if not needed.
   * @return The generated name of the parameter.
   */
  def addParam(hint: String = null): MuName = {
    val paramName = paramNamer.nextName(hint)
    funcDef.params.add(paramName)
    paramName
  }

  /**
   * Create a new basic block and add to the funcDef.
   * @param hint: A human-readable suffix of the basic block name, or null if not needed.
   * @return The basic block.
   */
  def newBB(hint: String = null): BasicBlock = {
    val bb = new BasicBlock()
    bb.name = bbNamer.nextName(hint)
    funcDef.bbs.add(bb)
    bb
  }

  /** The entry block. */
  @BeanProperty
  val entry = newBB("entry")

  @BeanProperty
  var curBB = entry

  /**
   * Add the instruction into the current basic block.
   * @param inst: The instruction to add
   */
  def emit(inst: Instruction) {
    curBB.insts.add(inst)
  }
}
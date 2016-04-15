package uvm.staticanalysis

import uvm._
import uvm.types._
import uvm.ssavariables._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object StaticAnalyzer {
  val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class StaticAnalyzer {
  import StaticAnalyzer._

  type MutableSet[T] = collection.mutable.HashSet[T]
  val MutableSet = collection.mutable.HashSet
  type MutableMap[K, V] = collection.mutable.HashMap[K, V]
  val MutableMap = collection.mutable.HashMap
  type MutableQueue[T] = collection.mutable.Queue[T]
  val MutableQueue = collection.mutable.Queue
  type MutableStack[T] = collection.mutable.Stack[T]
  val MutableStack = collection.mutable.Stack

  def checkBundle(bundle: Bundle, parentBundle: Option[GlobalBundle]): Unit = {
    new BundleChecker(bundle, parentBundle).check()
  }

  class BundleChecker(bundle: Bundle, parentBundle: Option[GlobalBundle]) {
    def check(): Unit = {
      checkTypes()
      checkSigs()
      checkConsts()
      checkGlobals()
      checkExpFuncs()
      checkFuncs()
    }

    def checkTypes(): Unit = {
      val compositeTypes = bundle.typeNs.all.flatMap {
        case ty: AbstractCompositeType => Some(ty)
        case _                         => None
      }.toSeq
      checkCompositeTypesNotRecursive(compositeTypes)
    }

    def checkCompositeTypesNotRecursive(compTys: Seq[AbstractCompositeType]): Unit = {
      val world = MutableSet(compTys: _*) // All types in this bundle. Assume all other types are valid.
      val visited = MutableSet[Type]()

      for (rootTy <- world if !visited.contains(rootTy)) {
        val inStack = MutableSet[AbstractCompositeType]()

        def visit(ty: AbstractCompositeType): Unit = {
          logger.debug("Checking composite type %s".format(ty.repr))
          visited(ty) = true
          inStack(ty) = true
          val succs = ty match {
            case TypeStruct(fieldTys)        => fieldTys
            case TypeArray(elemTy, _)        => Seq(elemTy)
            case TypeVector(elemTy, _)       => Seq(elemTy)
            case TypeHybrid(fixedTys, varTy) => fixedTys ++ Seq(varTy)
          }

          succs foreach {
            case succ @ TypeHybrid(fixedTys, varTy) =>
              throw error("Composite type %s contains hybrid %s".format(ty.repr, succ.repr),
                pretty = Seq(ty, succ))
            case succ @ TypeVoid() =>
              throw error("Composite type %s contains void %s".format(ty.repr, succ.repr),
                pretty = Seq(ty, succ))
            case succ: AbstractCompositeType => {
              if (inStack(succ)) {
                throw error("Composite type %s contains itself or its parent %s".format(ty.repr, succ.repr),
                  pretty = Seq(ty, succ))
              } else if (!visited(succ) && world.contains(succ)) {
                visit(succ)
              } else {
                // Ignore visited or out-of-bundle types.
              }
            }
            case _ => // do nothing if it is not composite
          }
          inStack(ty) = false
        }

        visit(rootTy)
      }
    }

    def checkValueType(ty: Type): Unit = {
      val invalidTypeKind = ty match {
        case _: TypeWeakRef => Some("weak reference")
        case _: TypeHybrid  => Some("hybrid")
        case _: TypeVoid    => Some("void")
        case _              => None
      }

      invalidTypeKind.foreach { kind =>
        throw error("%s %s cannot be the type of an SSA variable".format(kind, ty.repr),
          pretty = Seq(ty))
      }
    }

    def checkSigs(): Unit = {
      for (sig <- bundle.funcSigNs.all) {
        checkSig(sig)
      }
    }

    def checkSig(sig: FuncSig): Unit = {
      for (ty <- sig.paramTys ++ sig.retTys) try {
        checkValueType(ty)
      } catch {
        case e: StaticCheckingException => throw error("In function signature %s: %s".format(sig.repr, e.getMessage),
          pretty = Seq(sig), cause = e.getCause)
      }
    }

    def checkConsts(): Unit = {
      for (c <- bundle.constantNs.all) {
        checkScalarConst(c)
      }
      val compositeConsts = bundle.constantNs.all.flatMap {
        case c: ConstSeq => Some(c)
        case _           => None
      }.toSeq
      checkCompositeConstantsNotRecursive(compositeConsts)
    }

    def checkScalarConst(c: Constant): Unit = {
      c match {
        case cc @ ConstInt(ty, _) => ty match {
          case TypeInt(_)      =>
          case TypeUPtr(_)     =>
          case TypeUFuncPtr(_) =>
          case _ => {
            throw error("Constant %s: int literal is not suitable for type %s".format(c.repr, ty.repr),
              pretty = Seq(c, ty))
          }
        }
        case cc @ ConstFloat(ty, _) => ty match {
          case TypeFloat() =>
          case _ => {
            throw error("Constant %s: float literal is not suitable for type %s".format(c.repr, ty.repr),
              pretty = Seq(c, ty))
          }
        }
        case cc @ ConstDouble(ty, _) => ty match {
          case TypeDouble() =>
          case _ => {
            throw error("Constant %s: double literal is not suitable for type %s".format(c.repr, ty.repr),
              pretty = Seq(c, ty))
          }
        }
        case cc @ ConstNull(ty) => ty match {
          case TypeRef(_)  =>
          case TypeIRef(_) =>
          case TypeWeakRef(_) => {
            throw error("Constant %s: type %s is a weakref, which cannot have values.".format(c.repr, ty.repr),
              pretty = Seq(c, ty))
          }
          case TypeFuncRef(_)       =>
          case TypeStackRef()       =>
          case TypeThreadRef()      =>
          case TypeFrameCursorRef() =>
          case _ => {
            throw error("Constant %s: NULL literal is not suitable for type %s".format(c.repr, ty.repr),
              pretty = Seq(c, ty))
          }
        }
        case cc @ ConstSeq(ty, elems) => // Ignore for now
      }
    }

    def checkCompositeConstantsNotRecursive(compConsts: Seq[ConstSeq]): Unit = {
      val world = MutableSet(compConsts: _*) // All ConstSeq instances in this bundle. Assume all other constants are valid.
      val visited = MutableSet[ConstSeq]()

      for (rootConst <- world if !visited.contains(rootConst)) {
        val inStack = MutableSet[ConstSeq]()

        def visit(c: ConstSeq): Unit = {
          logger.debug("Checking composite constant %s".format(c.repr))
          visited(c) = true
          inStack(c) = true
          val succs = c match {
            case ConstSeq(ty, elems) => {
              val expectedArity = ty match {
                case t @ TypeStruct(fieldTys)   => fieldTys.length
                case t @ TypeArray(elemTy, sz)  => sz
                case t @ TypeVector(elemTy, sz) => sz
                case _ => throw error("Constant %s: sequence literal is not suitable for type %s".format(c.repr, ty.repr),
                  pretty = Seq(c, ty))
              }

              val actualArity = elems.length
              if (actualArity != expectedArity) {
                throw error("Constant %s: type %s expects %d elements, but %d found".format(c.repr, ty.repr,
                  expectedArity, actualArity), pretty = Seq(c, ty))
              }

              elems
            }
          }

          succs foreach {
            case succ: ConstSeq => {
              if (inStack(succ)) {
                throw error("Constant %s contains itself or its parent %s".format(c.repr, succ.repr),
                  pretty = Seq(c, succ))
              } else if (!visited.contains(succ) && world.contains(succ)) {
                visit(succ)
              } else {
                // Ignore visited or out-of-bundle types.
              }
            }
            case _ => // do nothing if it is not composite
          }
          inStack(c) = false
        }

        visit(rootConst)
      }
    }

    def lookupSourceInfo(obj: AnyRef): SourceInfo = {
      val si1 = bundle.sourceInfoRepo(obj)
      if (si1 == NoSourceInfo && parentBundle.isDefined) {
        return parentBundle.get.sourceInfoRepo(obj)
      } else {
        return si1
      }
    }

    def error(msg: String, pretty: Seq[AnyRef] = Seq(), cause: Throwable = null): StaticCheckingException = {
      val prettyMsgs = pretty.map(o => lookupSourceInfo(o).prettyPrint())
      new StaticCheckingException("%s\n%s".format(msg, prettyMsgs.mkString("\n")), cause)
    }

    def checkGlobals(): Unit = {
      for (g <- bundle.globalCellNs.all) {
        g.cellTy match {
          case ty: TypeVoid => throw error("Global cell %s: Global cell cannot have void type.".format(g.repr),
            pretty = Seq(g, ty))
          case ty: TypeHybrid => throw error("Global cell %s: Global cell cannot have hybrid type.".format(g.repr),
            pretty = Seq(g, ty))
        }
      }
    }

    def checkExpFuncs(): Unit = {
      for (ef <- bundle.expFuncNs.all) {
        ef.cookie.constTy match {
          case TypeInt(64) =>
          case ty => throw error("Exposed function %s: cookie must be a 64-bit int. %s found.".format(ef.repr, ty.repr),
            pretty = Seq(ef, ty))
        }
      }
    }

    def checkFuncs(): Unit = {
      for (fv <- bundle.funcVerNs.all) {
        checkFuncVer(fv)
      }
    }

    def checkFuncVer(fv: FuncVer): Unit = {
      val sig = fv.sig
      val fsig = fv.func.sig
      if (fsig.paramTys.length != sig.paramTys.length || fsig.retTys.length != sig.retTys.length) {
        throw error("Function version %s has different parameter or return value arity as its function %s".format(
          fv.repr, fv.func.repr), pretty = Seq(fv, sig, fv.func, fsig))
      }

      val entry = fv.entry
      if (entry.norParams.length != sig.paramTys.length) {
        throw error("Function version %s: the entry block has %d parameters, but the function takes %s parameters."
          .format(fv.repr, entry.norParams.length, sig.paramTys.length),
          pretty = Seq(fv, entry, sig))
      }

      if (entry.excParam.isDefined) {
        throw error("Function version %s: the entry block should not have exceptional parameter."
          .format(fv.repr),
          pretty = Seq(fv, entry))
      }

      for (bb <- fv.bbs) {
        checkBasicBlock(fv, entry, bb)
      }
    }

    def checkBasicBlock(fv: FuncVer, entry: BasicBlock, bb: BasicBlock): Unit = {
      if (bb.insts.isEmpty) {
        throw error("Function version %s: basic block %s is empty"
          .format(fv.repr, bb.repr),
          pretty = Seq(fv, bb))
      }
      val lastInst = bb.insts.last match {
        case i: MaybeTerminator if i.canTerminate => i
        case i => throw error("FuncVer %s BB %s: The last instruction %s is not a valid basic block terminator"
          .format(fv.repr, bb.repr, i.repr),
          pretty = Seq(fv, bb, i))
      }

      for ((dest, isNormal) <- bbDests(lastInst)) {
        if (dest.bb == entry) {
          throw error("FuncVer %s BB %s Inst %s: Cannot branch to the entry block"
            .format(fv.repr, bb.repr, lastInst.repr),
            pretty = Seq(fv, bb, lastInst))
        }

        val destBB = dest.bb
        val nParams = destBB.norParams.length
        val nArgs = dest.args.length
        if (nParams != nArgs) {
          throw error(("FuncVer %s BB %s Inst %s: Destination %s has %d normal parameters, but %d arguments found.\n" +
            "DestClause: %s")
            .format(fv.repr, bb.repr, lastInst.repr, destBB.repr, nParams, nArgs, dest),
            pretty = Seq(lastInst, destBB))
        }

        if (isNormal) {
          if (destBB.excParam.isDefined) {
            throw error(("FuncVer %s BB %s Inst %s: Normal destination %s should not have exceptional parameter.\n" +
              "DestClause: %s")
              .format(fv.repr, bb.repr, lastInst.repr, destBB.repr, dest),
              pretty = Seq(lastInst, destBB))
          }
        } else {
          if (!destBB.excParam.isDefined) {
            throw error(("FuncVer %s BB %s Inst %s: Exceptional destination %s must have exceptional parameter.\n" +
              "DestClause: %s")
              .format(fv.repr, bb.repr, lastInst.repr, destBB.repr, dest),
              pretty = Seq(lastInst, destBB))
          }
        }
      }
    }

    def bbDests(lastInst: MaybeTerminator): Seq[(DestClause, Boolean)] = lastInst match {
      case i: InstBranch     => Seq(i.dest).map(d => (d, true))
      case i: InstBranch2    => Seq(i.ifTrue, i.ifFalse).map(d => (d, true))
      case i: InstSwitch     => i.cases.map(_._2).map(d => (d, true))
      case i: InstTailCall   => Seq()
      case i: InstRet        => Seq()
      case i: InstThrow      => Seq()
      case i: InstWatchPoint => Seq(i.dis, i.ena).map(d => (d, true)) ++ i.exc.map(d => (d, false)).toSeq
      case i: InstWPBranch   => Seq(i.dis, i.ena).map(d => (d, true))
      case i: HasExcClause   => i.excClause.map(e => Seq((e.nor, true), (e.exc, false))).getOrElse(Seq())
    }
  }
}

class StaticCheckingException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

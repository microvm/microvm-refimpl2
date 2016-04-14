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
  }
}

class StaticCheckingException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

package aoc2025.utils

import scala.annotation.MacroAnnotation
import scala.quoted.*

class cached extends MacroAnnotation:
  def transform(using Quotes)(
      definition: quotes.reflect.Definition,
      companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*

    definition match
      case defdef @ DefDef(name, paramClauses, tpt, Some(rhs)) =>
        // Extract parameters from the clauses
        val valDefs = paramClauses.flatMap {
          case TermParamClause(params) => params
          case _ => Nil
        }

        if valDefs.isEmpty then
          report.error("@cached requires at least one parameter")
          return List(definition)

        val returnTpe = tpt.tpe

        // Build the key type and expressions based on parameter count
        valDefs.map(_.tpt.tpe) match
          case List(t1) =>
            t1.asType match
              case '[k] =>
                returnTpe.asType match
                  case '[v] =>
                    createCached1[k, v](defdef, name, paramClauses, tpt, rhs, valDefs)

          case List(t1, t2) =>
            ((t1.asType, t2.asType): @unchecked) match
              case ('[k1], '[k2]) =>
                returnTpe.asType match
                  case '[v] =>
                    createCached2[k1, k2, v](defdef, name, paramClauses, tpt, rhs, valDefs)

          case List(t1, t2, t3) =>
            ((t1.asType, t2.asType, t3.asType): @unchecked) match
              case ('[k1], '[k2], '[k3]) =>
                returnTpe.asType match
                  case '[v] =>
                    createCached3[k1, k2, k3, v](defdef, name, paramClauses, tpt, rhs, valDefs)

          case List(t1, t2, t3, t4) =>
            ((t1.asType, t2.asType, t3.asType, t4.asType): @unchecked) match
              case ('[k1], '[k2], '[k3], '[k4]) =>
                returnTpe.asType match
                  case '[v] =>
                    createCached4[k1, k2, k3, k4, v](defdef, name, paramClauses, tpt, rhs, valDefs)

          case _ =>
            report.error("@cached supports up to 4 parameters")
            List(definition)

      case _ =>
        report.error("@cached can only be applied to def methods")
        List(definition)

  private def createCached1[K: Type, V: Type](using Quotes)(
      defdef: quotes.reflect.DefDef,
      name: String,
      paramClauses: List[quotes.reflect.ParamClause],
      tpt: quotes.reflect.TypeTree,
      rhs: quotes.reflect.Term,
      valDefs: List[quotes.reflect.ValDef]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    val List(v1) = valDefs: @unchecked
    val keyExpr = Ref(v1.symbol).asExprOf[K]
    buildCachedMethod[K, V](defdef, name, paramClauses, tpt, rhs, keyExpr)

  private def createCached2[K1: Type, K2: Type, V: Type](using Quotes)(
      defdef: quotes.reflect.DefDef,
      name: String,
      paramClauses: List[quotes.reflect.ParamClause],
      tpt: quotes.reflect.TypeTree,
      rhs: quotes.reflect.Term,
      valDefs: List[quotes.reflect.ValDef]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    val List(v1, v2) = valDefs: @unchecked
    val keyExpr = '{ (${Ref(v1.symbol).asExprOf[K1]}, ${Ref(v2.symbol).asExprOf[K2]}) }
    buildCachedMethod[(K1, K2), V](defdef, name, paramClauses, tpt, rhs, keyExpr)

  private def createCached3[K1: Type, K2: Type, K3: Type, V: Type](using Quotes)(
      defdef: quotes.reflect.DefDef,
      name: String,
      paramClauses: List[quotes.reflect.ParamClause],
      tpt: quotes.reflect.TypeTree,
      rhs: quotes.reflect.Term,
      valDefs: List[quotes.reflect.ValDef]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    val List(v1, v2, v3) = valDefs: @unchecked
    val keyExpr = '{ (${Ref(v1.symbol).asExprOf[K1]}, ${Ref(v2.symbol).asExprOf[K2]}, ${Ref(v3.symbol).asExprOf[K3]}) }
    buildCachedMethod[(K1, K2, K3), V](defdef, name, paramClauses, tpt, rhs, keyExpr)

  private def createCached4[K1: Type, K2: Type, K3: Type, K4: Type, V: Type](using Quotes)(
      defdef: quotes.reflect.DefDef,
      name: String,
      paramClauses: List[quotes.reflect.ParamClause],
      tpt: quotes.reflect.TypeTree,
      rhs: quotes.reflect.Term,
      valDefs: List[quotes.reflect.ValDef]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    val List(v1, v2, v3, v4) = valDefs: @unchecked
    val keyExpr = '{ (${Ref(v1.symbol).asExprOf[K1]}, ${Ref(v2.symbol).asExprOf[K2]}, ${Ref(v3.symbol).asExprOf[K3]}, ${Ref(v4.symbol).asExprOf[K4]}) }
    buildCachedMethod[(K1, K2, K3, K4), V](defdef, name, paramClauses, tpt, rhs, keyExpr)

  private def buildCachedMethod[K: Type, V: Type](using Quotes)(
      defdef: quotes.reflect.DefDef,
      name: String,
      paramClauses: List[quotes.reflect.ParamClause],
      tpt: quotes.reflect.TypeTree,
      rhs: quotes.reflect.Term,
      keyExpr: Expr[K]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*

    val cacheName = Symbol.freshName(s"${name}$$cache")

    val cacheSymbol = Symbol.newVal(
      defdef.symbol.owner,
      cacheName,
      TypeRepr.of[scala.collection.mutable.Map[K, V]],
      Flags.Private,
      Symbol.noSymbol
    )

    val cacheInit = '{ scala.collection.mutable.Map.empty[K, V] }.asTerm
    val cacheValDef = ValDef(cacheSymbol, Some(cacheInit))

    val cacheRef = Ref(cacheSymbol).asExprOf[scala.collection.mutable.Map[K, V]]

    // Create a local function to properly capture method parameters in lambdas
    val computeSymbol = Symbol.newMethod(
      defdef.symbol,
      Symbol.freshName("compute"),
      MethodType(Nil)(_ => Nil, _ => TypeRepr.of[V])
    )

    // Re-parent the original rhs to the compute function
    val computeDef = DefDef(computeSymbol, _ => Some(rhs.changeOwner(computeSymbol)))
    val computeCall = Apply(Ref(computeSymbol), Nil).asExprOf[V]

    // Build: { def compute() = <rhs>; cache.getOrElseUpdate(key, compute()) }
    val newBody = Block(
      List(computeDef),
      '{ $cacheRef.getOrElseUpdate($keyExpr, $computeCall) }.asTerm
    )

    val newDefDef = DefDef.copy(defdef)(name, paramClauses, tpt, Some(newBody))

    List(cacheValDef, newDefDef)

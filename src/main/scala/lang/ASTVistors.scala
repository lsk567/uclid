package uclid
package lang

trait FoldingASTVisitor[T] {
  def applyOnModule(module : UclModule, in : T) : T = { in }
  def applyOnDecl(decl : UclDecl, in : T) : T = { in }
  def applyOnProcedure(proc : UclProcedureDecl, in : T) : T = { in }
  def applyOnFunction(func : UclFunctionDecl, in : T) : T = { in }
  def applyOnStateVar(stvar : UclStateVarDecl, in : T) : T = { in }
  def applyOnInputVar(inpvar : UclInputVarDecl, in : T) : T = { in }
  def applyOnOutputVar(outvar : UclOutputVarDecl, in : T) : T = { in }
  def applyOnConstant(cnst : UclConstantDecl, in : T) : T = { in }
  def applyOnSpec(spec : UclSpecDecl, in : T) : T = { in }
  def applyOnTypeDecl(typDec : UclTypeDecl, in : T) : T = { in }
  def applyOnInit(init : UclInitDecl, in : T) : T = { in }
  def applyOnNext(next : UclNextDecl, in : T) : T = { in }
  def applyOnType(typ: UclType, in : T) : T = { in }
  def applyOnProcedureSig(sig : UclProcedureSig, in : T) : T = { in }
  def applyOnFunctionSig(sig : UclFunctionSig, in : T) : T = { in }
  def applyOnLocalVar(lvar : UclLocalVarDecl, in : T) : T = { in }
  def applyOnStatement(st : UclStatement, in : T) : T = { in }
  def applyOnSkip(st : UclSkipStmt, in : T) : T = { in }
  def applyOnAssert(st : UclAssertStmt, in : T) : T = { in }
  def applyOnAssume(st : UclAssumeStmt, in : T) : T = { in }
  def applyOnHavoc(st : UclHavocStmt, in : T) : T = { in }
  def applyOnAssign(st : UclAssignStmt, in : T) : T = { in }
  def applyOnIfElse(st : UclIfElseStmt, in : T) : T = { in }
  def applyOnFor(st : UclForStmt, in : T) : T = { in }
  def applyOnCase(st : UclCaseStmt, in : T) : T = { in }
  def applyOnProcedureCall(st : UclProcedureCallStmt, in : T) : T = { in }
  def applyOnLHS(lhs : UclLhs, in : T) : T = { in }
  def applyOnExpr(e : Expr, in : T) : T = { in }
  def applyOnIdentifier(id : Identifier, in : T) : T = { in }
  def applyOnLit(lit : Literal, in : T) : T = { in }
  def applyOnBoolLit(b : BoolLit, in : T) : T = { in }
  def applyOnIntLit(i : IntLit, in : T) : T = { in }
  def applyOnBitVectorLit(bv : BitVectorLit, in : T) : T = { in }
  def applyOnRecord(rec : Record, in : T) : T = { in }
  def applyOnOperatorApp(opapp : UclOperatorApplication, in : T) : T = { in }
  def applyOnOperator(op : Operator, in : T) : T = { in }
  def applyOnArraySelect(arrSel : UclArraySelectOperation, in : T) : T = { in }
  def applyOnArrayStore(arrStore : UclArrayStoreOperation, in : T) : T = { in }
  def applyOnFuncApp(fapp : UclFuncApplication, in : T) : T = { in }
  def applyOnITE(ite : UclITE, in : T) : T = { in }
  def applyOnLambda(lambda : UclLambda, in : T) : T = { in }
}

trait RewritingASTVisitor {
  def rewriteModule(module : UclModule) : Option[UclModule] = { Some(module) }
  def rewriteDecl(decl : UclDecl) : Option[UclDecl] = { Some(decl) }
  def rewriteProcedure(proc : UclProcedureDecl) : Option[UclProcedureDecl] = { Some(proc) }
  def rewriteFunction(func : UclFunctionDecl) : Option[UclFunctionDecl] = { Some(func) }
  def rewriteStateVar(stvar : UclStateVarDecl) : Option[UclStateVarDecl] = { Some(stvar) }
  def rewriteInputVar(inpvar : UclInputVarDecl) : Option[UclInputVarDecl] = { Some(inpvar) }
  def rewriteOutputVar(outvar : UclOutputVarDecl) : Option[UclOutputVarDecl] = { Some(outvar) }
  def rewriteConstant(cnst : UclConstantDecl) : Option[UclConstantDecl] = { Some(cnst) }
  def rewriteSpec(spec : UclSpecDecl) : Option[UclSpecDecl] = { Some(spec) }
  def rewriteTypeDecl(typDec : UclTypeDecl) : Option[UclTypeDecl] = { Some(typDec) }
  def rewriteInit(init : UclInitDecl) : Option[UclInitDecl] = { Some(init) }
  def rewriteNext(next : UclNextDecl) : Option[UclNextDecl] = { Some(next) }
  def rewriteType(typ: UclType) : Option[UclType] = { Some(typ) }
  def rewriteProcedureSig(sig : UclProcedureSig) : Option[UclProcedureSig] = { Some(sig) }
  def rewriteFunctionSig(sig : UclFunctionSig) : Option[UclFunctionSig] = { Some(sig) }
  def rewriteLocalVar(lvar : UclLocalVarDecl) : Option[UclLocalVarDecl] = { Some(lvar) }
  def rewriteStatement(st : UclStatement) : Option[UclStatement] = { Some(st) }
  def rewriteSkip(st : UclSkipStmt) : Option[UclSkipStmt] = { Some(st) }
  def rewriteAssert(st : UclAssertStmt) : Option[UclAssertStmt] = { Some(st) }
  def rewriteAssume(st : UclAssumeStmt) : Option[UclAssumeStmt] = { Some(st) }
  def rewriteHavoc(st : UclHavocStmt) : Option[UclHavocStmt] = { Some(st) }
  def rewriteAssign(st : UclAssignStmt) : Option[UclAssignStmt] = { Some(st) }
  def rewriteIfElse(st : UclIfElseStmt) : Option[UclIfElseStmt] = { Some(st) }
  def rewriteFor(st : UclForStmt) : Option[UclForStmt] = { Some(st) }
  def rewriteCase(st : UclCaseStmt) : Option[UclCaseStmt] = { Some(st) }
  def rewriteProcedureCall(st : UclProcedureCallStmt) : Option[UclProcedureCallStmt] = { Some(st) }
  def rewriteLHS(lhs : UclLhs) : Option[UclLhs] = { Some(lhs) }
  def rewriteExpr(e : Expr) : Option[Expr] = { Some(e) }
  def rewriteIdentifier(id : Identifier) : Option[Identifier] = { Some(id) }
  def rewriteLit(lit : Literal) : Option[Literal] = { Some(lit) }
  def rewriteBoolLit(b : BoolLit) : Option[BoolLit] = { Some(b) }
  def rewriteIntLit(i : IntLit) : Option[IntLit] = { Some(i) }
  def rewriteBitVectorLit(bv : BitVectorLit) : Option[BitVectorLit] = { Some(bv) }
  def rewriteRecord(rec : Record) : Option[Record] = { Some(rec) }
  def rewriteOperatorApp(opapp : UclOperatorApplication) : Option[UclOperatorApplication] = { Some(opapp) }
  def rewriteOperator(op : Operator) : Option[Operator] = { Some(op) }
  def rewriteArraySelect(arrSel : UclArraySelectOperation) : Option[UclArraySelectOperation] = { Some(arrSel) }
  def rewriteArrayStore(arrStore : UclArrayStoreOperation) : Option[UclArrayStoreOperation] = { Some(arrStore) }
  def rewriteFuncApp(fapp : UclFuncApplication) : Option[UclFuncApplication] = { Some(fapp) }
  def rewriteITE(ite : UclITE) : Option[UclITE] = { Some(ite) }
  def rewriteLambda(lambda : UclLambda) : Option[UclLambda] = { Some(lambda) }
}


class FoldingVisitor[T] (v: FoldingASTVisitor[T], depthFirst: Boolean) {
  def visitModule(module : UclModule, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnModule(module, result)
    result = visitIdentifier(module.id, result)
    result = module.decls.foldLeft(result)((acc, i) => visitDecl(i, acc))
    if(depthFirst) result = v.applyOnModule(module, result)
    return result
  }
  def visitDecl(decl : UclDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnDecl(decl, result)
    result = decl match {
      case UclProcedureDecl(id, sig, vars, body) => visitProcedure(decl.asInstanceOf[UclProcedureDecl], result)
      case UclTypeDecl(id, typ) => visitTypeDecl(decl.asInstanceOf[UclTypeDecl], result)
      case UclStateVarDecl(id, typ) => visitStateVar(decl.asInstanceOf[UclStateVarDecl], result)
      case UclInputVarDecl(id, typ) => visitInputVar(decl.asInstanceOf[UclInputVarDecl], result)
      case UclOutputVarDecl(id, typ) => visitOutputVar(decl.asInstanceOf[UclOutputVarDecl], result)
      case UclConstantDecl(id, typ) => visitConstant(decl.asInstanceOf[UclConstantDecl], result)
      case UclFunctionDecl(id, sig) => visitFunction(decl.asInstanceOf[UclFunctionDecl], result)
      case UclInitDecl(body) => visitInit(decl.asInstanceOf[UclInitDecl], result)
      case UclNextDecl(body) => visitNext(decl.asInstanceOf[UclNextDecl], result)
      case UclSpecDecl(id, expr) => visitSpec(decl.asInstanceOf[UclSpecDecl], result)
    }
    if(depthFirst) result = v.applyOnDecl(decl, result)
    return result
  }
  def visitProcedure(proc : UclProcedureDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnProcedure(proc, result)
    result = visitIdentifier(proc.id, result)
    result = visitProcedureSig(proc.sig, result)
    result = proc.decls.foldLeft(result)((acc, i) => visitLocalVar(i, acc))
    result = proc.body.foldLeft(result)((acc, i) => visitStatement(i, acc))
    if(depthFirst) result = v.applyOnProcedure(proc, result)
    return result
  }
  def visitFunction(func : UclFunctionDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFunction(func, result)
    result = visitIdentifier(func.id, result)
    result = visitFunctionSig(func.sig, result)
    if(depthFirst) result = v.applyOnFunction(func, result)
    return result
  }
  def visitStateVar(stvar : UclStateVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnStateVar(stvar, result)
    result = visitIdentifier(stvar.id, result)
    result = visitType(stvar.typ, result)
    if(depthFirst) result = v.applyOnStateVar(stvar, result)
    return result
  }
  def visitInputVar(inpvar : UclInputVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnInputVar(inpvar, result)
    result = visitIdentifier(inpvar.id, result)
    result = visitType(inpvar.typ, result)
    if(depthFirst) result = v.applyOnInputVar(inpvar, result)
    return result
  }
  def visitOutputVar(outvar : UclOutputVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnOutputVar(outvar, result)
    result = visitIdentifier(outvar.id, result)
    result = visitType(outvar.typ, result)
    if(depthFirst) result = v.applyOnOutputVar(outvar, result)
    return result
  }
  def visitConstant(cnst : UclConstantDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnConstant(cnst, result)
    result = visitIdentifier(cnst.id, result)
    result = visitType(cnst.typ, result)
    if(depthFirst) result = v.applyOnConstant(cnst, result)
    return result
  }
  def visitSpec(spec : UclSpecDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnSpec(spec, result)
    result = visitIdentifier(spec.id, result)
    result = visitExpr(spec.expr, result)
    if(depthFirst) result = v.applyOnSpec(spec, result)
    return result
  }
  def visitTypeDecl(typDec : UclTypeDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnTypeDecl(typDec, result)
    result = visitIdentifier(typDec.id, result)
    result = visitType(typDec.typ, result)
    if(depthFirst) result = v.applyOnTypeDecl(typDec, result)
    return result
  }
  def visitInit(init : UclInitDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnInit(init, result)
    result = init.body.foldLeft(result)((acc, i) => visitStatement(i, acc))
    if(depthFirst) result = v.applyOnInit(init, result)
    return result
  }
  def visitNext(next : UclNextDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnNext(next, result)
    result = next.body.foldLeft(result)((acc, i) => visitStatement(i, acc))
    if(depthFirst) result = v.applyOnNext(next, result)
    return result
  }

  def visitType(typ: UclType, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnType(typ, result)
    if(depthFirst) result = v.applyOnType(typ, result)
    return result
  }

  def visitProcedureSig(sig : UclProcedureSig, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnProcedureSig(sig, result)
    result = sig.inParams.foldLeft(result)((acc, inparam) => visitIdentifier(inparam._1, acc))
    result = sig.inParams.foldLeft(result)((acc, inparam) => visitType(inparam._2, acc))
    result = sig.outParams.foldLeft(result)((acc, outparam) => visitIdentifier(outparam._1, acc))
    result = sig.outParams.foldLeft(result)((acc, outparam) => visitType(outparam._2, acc))
    if(depthFirst) result = v.applyOnProcedureSig(sig, result)
    return result
  }
  def visitFunctionSig(sig : UclFunctionSig, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFunctionSig(sig, result)
    result = sig.args.foldLeft(result)((acc, arg) => visitIdentifier(arg._1, acc))
    result = sig.args.foldLeft(result)((acc, arg) => visitType(arg._2, acc))
    result = visitType(sig.retType, result)
    if(depthFirst) result = v.applyOnFunctionSig(sig, result)
    return result
  }
  def visitLocalVar(lvar : UclLocalVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLocalVar(lvar, result)
    if(depthFirst) result = v.applyOnLocalVar(lvar, result)
    return result
  }
  def visitStatement(st : UclStatement, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnStatement(st, result)
    result = st match {
      case UclSkipStmt() => visitSkipStatement(st.asInstanceOf[UclSkipStmt], result)
      case UclAssertStmt(e) => visitAssertStatement(st.asInstanceOf[UclAssertStmt], result)
      case UclAssumeStmt(e) => visitAssumeStatement(st.asInstanceOf[UclAssumeStmt], result)
      case UclHavocStmt(id) => visitHavocStatement(st.asInstanceOf[UclHavocStmt], result)
      case UclAssignStmt(lhss, rhss) => visitAssignStatement(st.asInstanceOf[UclAssignStmt], result)
      case UclIfElseStmt(cond, ifblock, elseblock) => visitIfElseStatement(st.asInstanceOf[UclIfElseStmt], result)
      case UclForStmt(id, range, body) => visitForStatement(st.asInstanceOf[UclForStmt], result)
      case UclCaseStmt(body) => visitCaseStatement(st.asInstanceOf[UclCaseStmt], result)
      case UclProcedureCallStmt(id, callLhss, args) => visitProcedureCallStatement(st.asInstanceOf[UclProcedureCallStmt], result)
    }
    if(depthFirst) result = v.applyOnStatement(st, result)
    return result
  }

  def visitSkipStatement(st : UclSkipStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnSkip(st, result)
    if(depthFirst) result = v.applyOnSkip(st, result)
    return result
  }
  def visitAssertStatement(st : UclAssertStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnAssert(st, result)
    result = visitExpr(st.e, result)
    if(depthFirst) result = v.applyOnAssert(st, result)
    return result
  }
  def visitAssumeStatement(st : UclAssumeStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnAssume(st, result)
    result = visitExpr(st.e, result)
    if(depthFirst) result = v.applyOnAssume(st, result)
    return result
  }
  def visitHavocStatement(st: UclHavocStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnHavoc(st, result)
    result = visitIdentifier(st.id, result)
    if(depthFirst) result = v.applyOnHavoc(st, result)
    return result
  }
  def visitAssignStatement(st : UclAssignStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnAssign(st, result)
    result = st.lhss.foldLeft(result)((arg, i) => visitLhs(i, arg))
    result = st.rhss.foldLeft(result)((arg, i) => visitExpr(i, arg))
    if(depthFirst) result = v.applyOnAssign(st, result)
    return result
  }
  def visitIfElseStatement(st : UclIfElseStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnIfElse(st, result)
    result = visitExpr(st.cond, result)
    result = st.ifblock.foldLeft(result)((arg, i) => visitStatement(i, arg))
    result = st.elseblock.foldLeft(result)((arg, i) => visitStatement(i, arg))
    if(depthFirst) result = v.applyOnIfElse(st, result)
    return result
  }
  def visitForStatement(st : UclForStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFor(st, result)
    result = visitIdentifier(st.id, result)
    result = visitLiteral(st.range._1, result)
    result = visitLiteral(st.range._2, result)
    result = st.body.foldLeft(result)((arg, i) => visitStatement(i, arg))
    if(depthFirst) result = v.applyOnFor(st, result)
    return result
  }
  def visitCaseStatement(st : UclCaseStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnCase(st, result)
    result = st.body.foldLeft(result)(
      (arg, cases) => {
        cases._2.foldLeft(visitExpr(cases._1, arg))((arg, i) => visitStatement(i, arg))
      }
    )
    if(depthFirst) result = v.applyOnCase(st, result)
    return result
  }
  def visitProcedureCallStatement(st : UclProcedureCallStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnProcedureCall(st, result)
    result = visitIdentifier(st.id, result)
    result = st.callLhss.foldLeft(result)((arg, i) => visitLhs(i, arg))
    result = st.args.foldLeft(result)((arg, i) => visitExpr(i, arg))
    if(depthFirst) result = v.applyOnProcedureCall(st, result)
    return result
  }
  def visitLhs(lhs : UclLhs, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLHS(lhs, result)
    result = lhs.arraySelect match {
      case Some(as) => as.foldLeft(result)((acc, i) => visitExpr(i, acc))
      case None => result
    }
    result = lhs.recordSelect match {
      case Some(rs) => rs.foldLeft(result)((acc, i) => visitIdentifier(i, acc))
      case None => result
    }
    if(depthFirst) result = v.applyOnLHS(lhs, result)
    return result
  }
  def visitExpr(e : Expr, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnExpr(e, result)
    result = e match {
      case i : Identifier => visitIdentifier(i, result)
      case lit : Literal => visitLiteral(lit, result)
      case rec : Record => visitRecord(rec, result)
      case opapp : UclOperatorApplication => visitOperatorApp(opapp, result)
      case arrSel : UclArraySelectOperation => visitArraySelectOp(arrSel, result)
      case arrUpd : UclArrayStoreOperation => visitArrayStoreOp(arrUpd, result)
      case fapp : UclFuncApplication => visitFuncApp(fapp, result)
      case ite : UclITE => visitITE(ite, result)
      case lambda : UclLambda => visitLambda(lambda, result)
    }
    if(depthFirst) result = v.applyOnExpr(e, result)
    return result
  }
  def visitIdentifier(id : Identifier, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnIdentifier(id, result)
    if(depthFirst) result = v.applyOnIdentifier(id, result)
    return result
  }
  def visitLiteral(lit : Literal, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLit(lit, result)
    result = lit match {
      case BoolLit(b) => visitBoolLiteral(lit.asInstanceOf[BoolLit], result)
      case IntLit(i) => visitIntLiteral(lit.asInstanceOf[IntLit], result)
      case BitVectorLit(bv, w) => visitBitVectorLiteral(lit.asInstanceOf[BitVectorLit], result)
    }
    if(depthFirst) result = v.applyOnLit(lit, result)
    return result
  }
  def visitBoolLiteral(b : BoolLit, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnBoolLit(b, result)
    if(depthFirst) result = v.applyOnBoolLit(b, result)
    return result
  }
  def visitIntLiteral(i : IntLit, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnIntLit(i, result)
    if(depthFirst) result = v.applyOnIntLit(i, result)
    return result
  }
  def visitBitVectorLiteral(bv : BitVectorLit, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnBitVectorLit(bv, result)
    if(depthFirst) result = v.applyOnBitVectorLit(bv, result)
    return result
  }
  def visitRecord(rec : Record, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnRecord(rec, result)
    result = rec.value.foldLeft(result)((acc, i) => visitExpr(i, acc))
    if(depthFirst) result = v.applyOnRecord(rec, result)
    return result
  }
  def visitOperatorApp(opapp : UclOperatorApplication, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnOperatorApp(opapp, result)
    result = visitOperator(opapp.op, result)
    result = opapp.operands.foldLeft(result)((acc, i) => visitExpr(i, acc))
    if(depthFirst) result = v.applyOnOperatorApp(opapp, result)
    return result
  }
  def visitOperator(op : Operator, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnOperator(op, result)
    if(depthFirst) result = v.applyOnOperator(op, result)
    return result
  }
  def visitArraySelectOp(arrSel : UclArraySelectOperation, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnArraySelect(arrSel, result)
    result = visitExpr(arrSel.e, result)
    result = arrSel.index.foldLeft(result)((acc, arg) => visitExpr(arg, acc))
    if(depthFirst) result = v.applyOnArraySelect(arrSel, result)
    return result
  }
  def visitArrayStoreOp(arrStore : UclArrayStoreOperation, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnArrayStore(arrStore, result)
    result = visitExpr(arrStore.e, result)
    result = arrStore.index.foldLeft(result)((acc, arg) => visitExpr(arg, acc))
    result = visitExpr(arrStore.value, result)
    if(depthFirst) result = v.applyOnArrayStore(arrStore, result)
    return result
  }
  def visitFuncApp(fapp : UclFuncApplication, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFuncApp(fapp, result)
    result = visitExpr(fapp.e, result)
    result = fapp.args.foldLeft(result)((acc, arg) => visitExpr(arg, acc))
    if(depthFirst) result = v.applyOnFuncApp(fapp, result)
    return result
  }
  def visitITE(ite: UclITE, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnITE(ite, result)
    result = visitExpr(ite.e, result)
    result = visitExpr(ite.t, result)
    result = visitExpr(ite.f, result)
    if(depthFirst) result = v.applyOnITE(ite, result)
    return result
  }
  def visitLambda(lambda: UclLambda, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLambda(lambda, result)
    result = lambda.ids.foldLeft(result)((acc, arg) => visitIdentifier(arg._1, acc))
    result = lambda.ids.foldLeft(result)((acc, arg) => visitType(arg._2, acc))
    result = visitExpr(lambda.e, result)
    if(depthFirst) result = v.applyOnLambda(lambda, result)
    return result
  }
}

class RewritingVisitor[T] (v: RewritingASTVisitor) {
  def visitModule(module : UclModule) : Option[UclModule] = {
    val id = visitIdentifier(module.id, result)
    val decls = module.decls.map(visitDecl(_)).flatten
    if (!id.isEmpty) {
      val module_p = UclModule(id, decls)
      return v.applyOnModule(module, result)
    } else {
      return None
    }
  }
  def visitDecl(decl : UclDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnDecl(decl, result)
    result = decl match {
      case UclProcedureDecl(id, sig, vars, body) => visitProcedure(decl.asInstanceOf[UclProcedureDecl], result)
      case UclTypeDecl(id, typ) => visitTypeDecl(decl.asInstanceOf[UclTypeDecl], result)
      case UclStateVarDecl(id, typ) => visitStateVar(decl.asInstanceOf[UclStateVarDecl], result)
      case UclInputVarDecl(id, typ) => visitInputVar(decl.asInstanceOf[UclInputVarDecl], result)
      case UclOutputVarDecl(id, typ) => visitOutputVar(decl.asInstanceOf[UclOutputVarDecl], result)
      case UclConstantDecl(id, typ) => visitConstant(decl.asInstanceOf[UclConstantDecl], result)
      case UclFunctionDecl(id, sig) => visitFunction(decl.asInstanceOf[UclFunctionDecl], result)
      case UclInitDecl(body) => visitInit(decl.asInstanceOf[UclInitDecl], result)
      case UclNextDecl(body) => visitNext(decl.asInstanceOf[UclNextDecl], result)
      case UclSpecDecl(id, expr) => visitSpec(decl.asInstanceOf[UclSpecDecl], result)
    }
    if(depthFirst) result = v.applyOnDecl(decl, result)
    return result
  }
  def visitProcedure(proc : UclProcedureDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnProcedure(proc, result)
    result = visitIdentifier(proc.id, result)
    result = visitProcedureSig(proc.sig, result)
    result = proc.decls.foldLeft(result)((acc, i) => visitLocalVar(i, acc))
    result = proc.body.foldLeft(result)((acc, i) => visitStatement(i, acc))
    if(depthFirst) result = v.applyOnProcedure(proc, result)
    return result
  }
  def visitFunction(func : UclFunctionDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFunction(func, result)
    result = visitIdentifier(func.id, result)
    result = visitFunctionSig(func.sig, result)
    if(depthFirst) result = v.applyOnFunction(func, result)
    return result
  }
  def visitStateVar(stvar : UclStateVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnStateVar(stvar, result)
    result = visitIdentifier(stvar.id, result)
    result = visitType(stvar.typ, result)
    if(depthFirst) result = v.applyOnStateVar(stvar, result)
    return result
  }
  def visitInputVar(inpvar : UclInputVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnInputVar(inpvar, result)
    result = visitIdentifier(inpvar.id, result)
    result = visitType(inpvar.typ, result)
    if(depthFirst) result = v.applyOnInputVar(inpvar, result)
    return result
  }
  def visitOutputVar(outvar : UclOutputVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnOutputVar(outvar, result)
    result = visitIdentifier(outvar.id, result)
    result = visitType(outvar.typ, result)
    if(depthFirst) result = v.applyOnOutputVar(outvar, result)
    return result
  }
  def visitConstant(cnst : UclConstantDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnConstant(cnst, result)
    result = visitIdentifier(cnst.id, result)
    result = visitType(cnst.typ, result)
    if(depthFirst) result = v.applyOnConstant(cnst, result)
    return result
  }
  def visitSpec(spec : UclSpecDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnSpec(spec, result)
    result = visitIdentifier(spec.id, result)
    result = visitExpr(spec.expr, result)
    if(depthFirst) result = v.applyOnSpec(spec, result)
    return result
  }
  def visitTypeDecl(typDec : UclTypeDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnTypeDecl(typDec, result)
    result = visitIdentifier(typDec.id, result)
    result = visitType(typDec.typ, result)
    if(depthFirst) result = v.applyOnTypeDecl(typDec, result)
    return result
  }
  def visitInit(init : UclInitDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnInit(init, result)
    result = init.body.foldLeft(result)((acc, i) => visitStatement(i, acc))
    if(depthFirst) result = v.applyOnInit(init, result)
    return result
  }
  def visitNext(next : UclNextDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnNext(next, result)
    result = next.body.foldLeft(result)((acc, i) => visitStatement(i, acc))
    if(depthFirst) result = v.applyOnNext(next, result)
    return result
  }

  def visitType(typ: UclType, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnType(typ, result)
    if(depthFirst) result = v.applyOnType(typ, result)
    return result
  }

  def visitProcedureSig(sig : UclProcedureSig, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnProcedureSig(sig, result)
    result = sig.inParams.foldLeft(result)((acc, inparam) => visitIdentifier(inparam._1, acc))
    result = sig.inParams.foldLeft(result)((acc, inparam) => visitType(inparam._2, acc))
    result = sig.outParams.foldLeft(result)((acc, outparam) => visitIdentifier(outparam._1, acc))
    result = sig.outParams.foldLeft(result)((acc, outparam) => visitType(outparam._2, acc))
    if(depthFirst) result = v.applyOnProcedureSig(sig, result)
    return result
  }
  def visitFunctionSig(sig : UclFunctionSig, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFunctionSig(sig, result)
    result = sig.args.foldLeft(result)((acc, arg) => visitIdentifier(arg._1, acc))
    result = sig.args.foldLeft(result)((acc, arg) => visitType(arg._2, acc))
    result = visitType(sig.retType, result)
    if(depthFirst) result = v.applyOnFunctionSig(sig, result)
    return result
  }
  def visitLocalVar(lvar : UclLocalVarDecl, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLocalVar(lvar, result)
    if(depthFirst) result = v.applyOnLocalVar(lvar, result)
    return result
  }
  def visitStatement(st : UclStatement, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnStatement(st, result)
    result = st match {
      case UclSkipStmt() => visitSkipStatement(st.asInstanceOf[UclSkipStmt], result)
      case UclAssertStmt(e) => visitAssertStatement(st.asInstanceOf[UclAssertStmt], result)
      case UclAssumeStmt(e) => visitAssumeStatement(st.asInstanceOf[UclAssumeStmt], result)
      case UclHavocStmt(id) => visitHavocStatement(st.asInstanceOf[UclHavocStmt], result)
      case UclAssignStmt(lhss, rhss) => visitAssignStatement(st.asInstanceOf[UclAssignStmt], result)
      case UclIfElseStmt(cond, ifblock, elseblock) => visitIfElseStatement(st.asInstanceOf[UclIfElseStmt], result)
      case UclForStmt(id, range, body) => visitForStatement(st.asInstanceOf[UclForStmt], result)
      case UclCaseStmt(body) => visitCaseStatement(st.asInstanceOf[UclCaseStmt], result)
      case UclProcedureCallStmt(id, callLhss, args) => visitProcedureCallStatement(st.asInstanceOf[UclProcedureCallStmt], result)
    }
    if(depthFirst) result = v.applyOnStatement(st, result)
    return result
  }

  def visitSkipStatement(st : UclSkipStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnSkip(st, result)
    if(depthFirst) result = v.applyOnSkip(st, result)
    return result
  }
  def visitAssertStatement(st : UclAssertStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnAssert(st, result)
    result = visitExpr(st.e, result)
    if(depthFirst) result = v.applyOnAssert(st, result)
    return result
  }
  def visitAssumeStatement(st : UclAssumeStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnAssume(st, result)
    result = visitExpr(st.e, result)
    if(depthFirst) result = v.applyOnAssume(st, result)
    return result
  }
  def visitHavocStatement(st: UclHavocStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnHavoc(st, result)
    result = visitIdentifier(st.id, result)
    if(depthFirst) result = v.applyOnHavoc(st, result)
    return result
  }
  def visitAssignStatement(st : UclAssignStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnAssign(st, result)
    result = st.lhss.foldLeft(result)((arg, i) => visitLhs(i, arg))
    result = st.rhss.foldLeft(result)((arg, i) => visitExpr(i, arg))
    if(depthFirst) result = v.applyOnAssign(st, result)
    return result
  }
  def visitIfElseStatement(st : UclIfElseStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnIfElse(st, result)
    result = visitExpr(st.cond, result)
    result = st.ifblock.foldLeft(result)((arg, i) => visitStatement(i, arg))
    result = st.elseblock.foldLeft(result)((arg, i) => visitStatement(i, arg))
    if(depthFirst) result = v.applyOnIfElse(st, result)
    return result
  }
  def visitForStatement(st : UclForStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFor(st, result)
    result = visitIdentifier(st.id, result)
    result = visitLiteral(st.range._1, result)
    result = visitLiteral(st.range._2, result)
    result = st.body.foldLeft(result)((arg, i) => visitStatement(i, arg))
    if(depthFirst) result = v.applyOnFor(st, result)
    return result
  }
  def visitCaseStatement(st : UclCaseStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnCase(st, result)
    result = st.body.foldLeft(result)(
      (arg, cases) => {
        cases._2.foldLeft(visitExpr(cases._1, arg))((arg, i) => visitStatement(i, arg))
      }
    )
    if(depthFirst) result = v.applyOnCase(st, result)
    return result
  }
  def visitProcedureCallStatement(st : UclProcedureCallStmt, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnProcedureCall(st, result)
    result = visitIdentifier(st.id, result)
    result = st.callLhss.foldLeft(result)((arg, i) => visitLhs(i, arg))
    result = st.args.foldLeft(result)((arg, i) => visitExpr(i, arg))
    if(depthFirst) result = v.applyOnProcedureCall(st, result)
    return result
  }
  def visitLhs(lhs : UclLhs, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLHS(lhs, result)
    result = lhs.arraySelect match {
      case Some(as) => as.foldLeft(result)((acc, i) => visitExpr(i, acc))
      case None => result
    }
    result = lhs.recordSelect match {
      case Some(rs) => rs.foldLeft(result)((acc, i) => visitIdentifier(i, acc))
      case None => result
    }
    if(depthFirst) result = v.applyOnLHS(lhs, result)
    return result
  }
  def visitExpr(e : Expr, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnExpr(e, result)
    result = e match {
      case i : Identifier => visitIdentifier(i, result)
      case lit : Literal => visitLiteral(lit, result)
      case rec : Record => visitRecord(rec, result)
      case opapp : UclOperatorApplication => visitOperatorApp(opapp, result)
      case arrSel : UclArraySelectOperation => visitArraySelectOp(arrSel, result)
      case arrUpd : UclArrayStoreOperation => visitArrayStoreOp(arrUpd, result)
      case fapp : UclFuncApplication => visitFuncApp(fapp, result)
      case ite : UclITE => visitITE(ite, result)
      case lambda : UclLambda => visitLambda(lambda, result)
    }
    if(depthFirst) result = v.applyOnExpr(e, result)
    return result
  }
  def visitIdentifier(id : Identifier, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnIdentifier(id, result)
    if(depthFirst) result = v.applyOnIdentifier(id, result)
    return result
  }
  def visitLiteral(lit : Literal, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLit(lit, result)
    result = lit match {
      case BoolLit(b) => visitBoolLiteral(lit.asInstanceOf[BoolLit], result)
      case IntLit(i) => visitIntLiteral(lit.asInstanceOf[IntLit], result)
      case BitVectorLit(bv, w) => visitBitVectorLiteral(lit.asInstanceOf[BitVectorLit], result)
    }
    if(depthFirst) result = v.applyOnLit(lit, result)
    return result
  }
  def visitBoolLiteral(b : BoolLit, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnBoolLit(b, result)
    if(depthFirst) result = v.applyOnBoolLit(b, result)
    return result
  }
  def visitIntLiteral(i : IntLit, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnIntLit(i, result)
    if(depthFirst) result = v.applyOnIntLit(i, result)
    return result
  }
  def visitBitVectorLiteral(bv : BitVectorLit, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnBitVectorLit(bv, result)
    if(depthFirst) result = v.applyOnBitVectorLit(bv, result)
    return result
  }
  def visitRecord(rec : Record, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnRecord(rec, result)
    result = rec.value.foldLeft(result)((acc, i) => visitExpr(i, acc))
    if(depthFirst) result = v.applyOnRecord(rec, result)
    return result
  }
  def visitOperatorApp(opapp : UclOperatorApplication, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnOperatorApp(opapp, result)
    result = visitOperator(opapp.op, result)
    result = opapp.operands.foldLeft(result)((acc, i) => visitExpr(i, acc))
    if(depthFirst) result = v.applyOnOperatorApp(opapp, result)
    return result
  }
  def visitOperator(op : Operator, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnOperator(op, result)
    if(depthFirst) result = v.applyOnOperator(op, result)
    return result
  }
  def visitArraySelectOp(arrSel : UclArraySelectOperation, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnArraySelect(arrSel, result)
    result = visitExpr(arrSel.e, result)
    result = arrSel.index.foldLeft(result)((acc, arg) => visitExpr(arg, acc))
    if(depthFirst) result = v.applyOnArraySelect(arrSel, result)
    return result
  }
  def visitArrayStoreOp(arrStore : UclArrayStoreOperation, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnArrayStore(arrStore, result)
    result = visitExpr(arrStore.e, result)
    result = arrStore.index.foldLeft(result)((acc, arg) => visitExpr(arg, acc))
    result = visitExpr(arrStore.value, result)
    if(depthFirst) result = v.applyOnArrayStore(arrStore, result)
    return result
  }
  def visitFuncApp(fapp : UclFuncApplication, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnFuncApp(fapp, result)
    result = visitExpr(fapp.e, result)
    result = fapp.args.foldLeft(result)((acc, arg) => visitExpr(arg, acc))
    if(depthFirst) result = v.applyOnFuncApp(fapp, result)
    return result
  }
  def visitITE(ite: UclITE, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnITE(ite, result)
    result = visitExpr(ite.e, result)
    result = visitExpr(ite.t, result)
    result = visitExpr(ite.f, result)
    if(depthFirst) result = v.applyOnITE(ite, result)
    return result
  }
  def visitLambda(lambda: UclLambda, in : T) : T = {
    var result : T = in
    if(!depthFirst) result = v.applyOnLambda(lambda, result)
    result = lambda.ids.foldLeft(result)((acc, arg) => visitIdentifier(arg._1, acc))
    result = lambda.ids.foldLeft(result)((acc, arg) => visitType(arg._2, acc))
    result = visitExpr(lambda.e, result)
    if(depthFirst) result = v.applyOnLambda(lambda, result)
    return result
  }
}

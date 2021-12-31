package uclid
package lang

class InitNextRemoverPass(moduleName : Identifier) extends RewritePass {
  //override def rewriteBlock(st : BlockStmt, ctx : Scope) : Option[Statement] = { Some(BlockStmt(List.empty[BlockVarsDecl], List.empty[Statement])) }
  override def rewriteModule(module : Module, ctx : Scope) : Option[Module] = {

    val otherDecls = module.decls.filter((d) => !d.isInstanceOf[InitDecl] && !d.isInstanceOf[NextDecl])

    if (module.id == moduleName) {
      // advance all instances

      val newNext = NextDecl(BlockStmt(List.empty[BlockVarsDecl], module.instances.map((inst) => ModuleCallStmt(inst.instanceId))))
      val newInit = InitDecl(SkipStmt())
      val newDecls = newInit :: newNext :: otherDecls
      val newModule = Module(module.id, newDecls, module.cmds, module.notes)
      Some(newModule)

    } else {
      val newNext = NextDecl(SkipStmt())
      val newInit = InitDecl(SkipStmt())
      val newDecls = newInit :: newNext :: otherDecls
      val newModule = Module(module.id, newDecls, module.cmds, module.notes)
      Some(newModule)
    }
    
    
      //UclidMain.println(newModule.toString)
  }

  //   override def rewriteAssign(next : NextDecl, ctx : Scope) : Option[NextDecl] = {
  //     next.body.vars.filter((st) => )
  //   	Some(ContractOperation.saturation(contract))
  //   }
  // }
  //   override def rewriteSkip(st : SkipStmt, ctx : Scope) : Option[Statement] = { None }
  //   override def rewriteAssert(st : AssertStmt, ctx : Scope) : Option[Statement] = { None }
  //   override def rewriteAssume(st : AssumeStmt, ctx : Scope) : Option[Statement] = { None }
  //   override def rewriteHavoc(st : HavocStmt, ctx : Scope) : Option[Statement] = { None }
  //   override def rewriteAssign(st : AssignStmt, ctx : Scope) : Option[Statement] = { None }
  //override def rewriteBlock(st : BlockStmt, ctx : Scope) : Option[Statement] = { Some(st) }
  //override def rewriteIfElse(st : IfElseStmt, ctx : Scope) : Option[Statement] = { Some(st) }
  //override def rewriteFor(st : ForStmt, ctx : Scope) : Option[Statement] = { Some(st) }
  //override def rewriteWhile(st : WhileStmt, ctx : Scope) : Option[Statement] = { Some(st) }
  //override def rewriteCase(st : CaseStmt, ctx : Scope) : Option[Statement] = { Some(st) }
  //override def rewriteProcedureCall(st : ProcedureCallStmt, ctx : Scope) : Option[Statement] = { Some(st) }
  //override def rewriteModuleCall(st : ModuleCallStmt, ctx : Scope) : Option[Statement] = { Some(st) }
}
class InitNextRemover(moduleName : Identifier) extends ASTRewriter(
    "InitNextRemover", new InitNextRemoverPass(moduleName))
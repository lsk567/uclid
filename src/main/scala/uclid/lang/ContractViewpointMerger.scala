package uclid
package lang

class ContractSaturatorPass() extends RewritePass {
  override def rewriteContract(contract : ContractDecl, ctx : Scope) : Option[ContractDecl] = {
  	Some(ContractOperation.saturation(contract))
  }
}

class ContractSaturator() extends ASTRewriter(
    "ContractSaturator", new ContractSaturatorPass())

class ContractViewpointMergerPass(moduleName : Identifier) extends RewritePass {
  override def rewriteModule(module : Module, ctx : Scope) : Option[Module] = {
    if (module.id == moduleName) {
      //UclidMain.println("Main module: "+ module.id.toString)
      Some(module)
    } else {
        val mergedagContractDecl = ContractOperation.merging(module.contracts, Identifier(module.id.toString() + "_system_level_contract"))
        val newDecls : List[Decl] = mergedagContractDecl :: module.decls.filter( (decl) => 
          decl match{
            case ContractDecl(_,_,_,_) => false
            case _ => true
          }
        )
        val newModule = Module(module.id, newDecls, module.cmds, module.notes)
        //UclidMain.println(newModule.toString)
		Some(newModule)
    }
  }
}

class ContractViewpointMerger(moduleName : Identifier) extends ASTRewriter(
    "ContractViewpointMerger", new ContractViewpointMergerPass(moduleName))
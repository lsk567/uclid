package uclid
package lang

class ContractViewpointMergerPass(moduleName : Identifier) extends RewritePass {
  override def rewriteModule(module : Module, ctx : Scope) : Option[Module] = {
    if (module.id == moduleName) {
      //UclidMain.println("Main module: "+ module.id.toString)
      Some(module)
    } else {
        val assumes = module.contracts.foldLeft(List.empty[Expr]){(acc, contract) => {
              contract.expr_a :: acc
            }
        }
        val guarantees = module.contracts.foldLeft(List.empty[Expr]){(acc, contract) => {
              contract.expr_g :: acc
            }
        }
        val assumeConjunction = assumes.reduceLeft((acc, assume) => {
            Operator.and(acc, assume)
          }
        )
        val guaranteeConjunction = guarantees.reduceLeft((acc, guarantee) => {
            Operator.and(acc, guarantee)
          }
        )
        val mergedagContractDecl = ContractDecl(Identifier(module.id.toString() + "_system_level_contract"), assumeConjunction , Operator.or(guaranteeConjunction, Operator.not(assumeConjunction)), List.empty)
        //UclidMain.println(mergedagContractDecl.toString)
        //todo : filter out contract decls in 
        val newContractDecls : List[Decl] = mergedagContractDecl :: module.decls.filter( (decl) => 
          decl match{
            case ContractDecl(_,_,_,_) => false
            case _ => true
          }
        )
        val newModule = Module(module.id, newContractDecls, module.cmds, module.notes)
        //UclidMain.println(newModule.toString)
		Some(newModule)
    }
  }
}

class ContractViewpointMerger(moduleName : Identifier) extends ASTRewriter(
    "ContractViewpointMerger", new ContractViewpointMergerPass(moduleName))
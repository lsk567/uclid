package uclid
package lang

object ContractOperation{
	def saturation(contract: ContractDecl): ContractDecl = {
		ContractDecl(contract.id, contract.expr_a , Operator.or(contract.expr_g, Operator.not(contract.expr_a)), contract.params)
	}
	//TODO: check if the contract is saturated
	//def isSaturated(contract: ContractDecl): Boolean

	def composition(contracts: List[ContractDecl], name: Identifier): ContractDecl = {
	    val assumes = contracts.foldLeft(List.empty[Expr]){(acc, contract) => {
	          contract.expr_a :: acc
	        }
	    }
	    val guarantees = contracts.foldLeft(List.empty[Expr]){(acc, contract) => {
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
	    val composedagContractDecl = ContractDecl(name, Operator.or(assumeConjunction, Operator.not(guaranteeConjunction)) , guaranteeConjunction, List.empty)
		composedagContractDecl
	}

	def quotient(sys_contract: ContractDecl, component_contract: ContractDecl, name: Identifier) = {
		val tmpg = Operator.and(sys_contract.expr_g, component_contract.expr_a)
		val tmpa = Operator.and(component_contract.expr_g, sys_contract.expr_a)
		ContractDecl(name, tmpa , Operator.or(tmpg, Operator.not(tmpa)), List.empty)
	}

	def separation(contract: ContractDecl, viewpoint_contract: ContractDecl, name: Identifier) = {
		val tmpg = Operator.and(contract.expr_g, viewpoint_contract.expr_a)
		val tmpa = Operator.and(viewpoint_contract.expr_g, contract.expr_a)
		ContractDecl(name, Operator.or(tmpa, Operator.not(tmpg)) , tmpg, List.empty)
	}

	def merging(contracts: List[ContractDecl], name: Identifier): ContractDecl = {
	    val assumes = contracts.foldLeft(List.empty[Expr]){(acc, contract) => {
	          contract.expr_a :: acc
	        }
	    }
	    val guarantees = contracts.foldLeft(List.empty[Expr]){(acc, contract) => {
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
	    val mergedagContractDecl = ContractDecl(name, assumeConjunction , Operator.or(guaranteeConjunction, Operator.not(assumeConjunction)), List.empty)
		mergedagContractDecl
	}
}
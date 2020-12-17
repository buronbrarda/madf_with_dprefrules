package java_ui.arguments;

import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;

public class Argument {

	
	private String id;
	private String claim;
	private String rule;
	private boolean accepted;
	
	
	public Argument(String id, String claim, String rule, boolean accepted){
		this.id = id;
		this.claim = claim;
		this.rule = rule;
		this.accepted = accepted;
	}
	
	
	public Argument(String id) {
		this.id = id;
		
		
		//Query to know the claim and the rule of the argument.

		Query qArg = new Query("claim("+id+",Claim), rule("+this.id+",RuleId)");

		if(qArg.hasNext()){
			
			Map<String,Term> solution = qArg.next();
			this.claim = solution.get("Claim").toString();
			this.rule = solution.get("RuleId").toString();		
		}
		
		
		//Query to know if it is an accepted argument, or not.
		Query q = new Query("dtree_node(_,null,_,"+this.id+",'U')");
		this.accepted = q.hasNext();
		while(q.hasNext())
			q.next();
	}


	public String getId(){
		return this.id;
	}
	
	public String getClaim(){
		return this.claim;
	}
	
	public String getRule(){
		return this.rule;
	}
	
	public boolean isAccepted() {
		return this.accepted;
	}
}

package java_ui.arguments;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;

public class Argument {

	
	private String id;
	private String claim;
	private ArrayList<String> rules;
	private boolean accepted;
	
	
	public Argument(String id, String claim, ArrayList<String> rules, boolean accepted){
		this.id = id;
		this.claim = claim;
		this.rules = rules;
		this.accepted = accepted;
	}
	
	
	public Argument(String id) {
		this.id = id;
		
		Query q_claim = new Query("claim("+id+",Claim)");
		
		if(q_claim.hasNext()){
			
			this.claim = q_claim.next().get("Claim").toString();
		}
		
		
		this.rules = new ArrayList<String>();
		for ( Map<String,Term> solution : new Query("rules("+this.id+",Rules), member(cpref_rule(Id,_), Rules)")) {
			this.rules.add(solution.get("Id").toString());	
		}
		
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
	
	public ArrayList<String> getRules(){
		return this.rules;
	}
	
	public boolean isAccepted() {
		return this.accepted;
	}
}

package java_ui.arguments;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class Argument {

	
	private String id;
	private String claim;
	private ArrayList<String> rules;
	private ArrayList<String> subargs;
	private boolean accepted;
	
	
	public Argument(String id, String claim, ArrayList<String> rules, ArrayList<String> subargs,boolean accepted){
		this.id = id;
		this.claim = claim;
		this.rules = rules;
		this.subargs = subargs;
		this.accepted = accepted;
	}
	
	
	public Argument(String id) {
		this.id = id;
		this.rules = new ArrayList<String>();
		
		//Query to know the claim and the rule of the argument.

		Query qArg = new Query("argument("+this.id+",RuleIds,Subargs,Claim)");

		for(Map<String,Term> solution : qArg){

			this.claim = solution.get("Claim").toString();

			Term [] aux = Util.listToTermArray(solution.get("RuleIds"));
			for(Term r : aux){
				this.rules.add(r.toString());
			}

			aux = Util.listToTermArray(solution.get("Subargs"));
			for(Term subarg : aux){
				this.rules.add(subarg.toString());
			}
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
	
	public ArrayList<String> getRules(){
		return this.rules;
	}

	public ArrayList<String> getSubargs(){
		return this.subargs;
	}
	
	public boolean isAccepted() {
		return this.accepted;
	}
}

package java_ui.arguments.dialectical_trees;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class ArgumentsGraphEdge {

	private int id;
	private String attacker;
	private String defender;
	private boolean successful;
	private ArrayList<String> explanation;
	
	public ArgumentsGraphEdge(String attacker, String defender){
		this.id = this.hashCode();
		
		this.attacker = attacker;
		this.defender = defender;
		this.successful = false; //By default it is false
		
		this.explanation = new ArrayList<String>();
		
		//Query to know if it is about a successful attack, or not.
		//That is, whether the attacker defeat the defender.
		Query q = new Query("defeats("+attacker+","+defender+")");
		while(q.hasNext()) {
			this.successful = true; //If the query has a solution, then it is a successful attack.
		}
		
		
		//Query to obtain the edge explanation
		q = new Query("defeat_explanation("+attacker+","+defender+",Explanation)");
	
		while(q.hasNext()){
			Map<String,Term> solution = q.next();
			String [] aux = Util.atomListToStringArray(solution.get("Explanation"));
			
			for(String s : aux) {
				this.explanation.add(s);
			}		
		}
	}
	
	public int getId(){
		return this.id;
	}

	public String getAttacker() {
		return this.attacker;
	}

	public String getDefender() {
		return this.defender;
	}

	public boolean isSuccessful() {
		return this.successful;
	}
	
	public String getExplanation(){
		String toReturn = "{";
		
		if(!this.explanation.isEmpty()){
			int i;
			
			for(i=0; i<this.explanation.size()-1;i++){
				toReturn += this.explanation.get(i)+", ";
			}
			
			
			toReturn += this.explanation.get(i);
		}
		
		return toReturn +"}";
	}
	
}

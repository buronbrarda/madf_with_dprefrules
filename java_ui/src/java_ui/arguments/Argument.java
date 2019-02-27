package java_ui.arguments;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class Argument {

	
	private int id;
	private String claim;
	private ArrayList<String> facts;
	private ArrayList<String> rules;
	
	
	public Argument(int id, String claim, ArrayList<String> facts, ArrayList<String> rules){
		this.id = id;
		this.claim = claim;
		this.facts = facts;
		this.rules = rules;		
	}
	
	
	public Argument(String id) {
		this.id = Integer.parseInt(id);
		
		Query q = new Query("argument("+this.id+",Rules,Facts,Claim)");
		
		if(q.hasNext()){
			Map<String, Term> s = q.next();
			
			this.rules = termArrayToArrayList(Util.listToTermArray(s.get("Rules")));
			this.facts = termArrayToArrayList(Util.listToTermArray(s.get("Facts")));
			
			this.claim = s.get("Claim").toString();
		}
	}


	private ArrayList<String> termArrayToArrayList(Term[] terms) {
		ArrayList<String> toReturn = new ArrayList<String>();
		
		for(Term t : terms){
			toReturn.add(t.toString());
		}
		
		return toReturn;
	}


	public int getId(){
		return this.id;
	}
	
	public String getClaim(){
		return this.claim;
	}
	
	public ArrayList<String> getFacts(){
		return this.facts;
	}
	
	public ArrayList<String> getRules(){
		return this.rules;
	}
}

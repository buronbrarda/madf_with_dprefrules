package java_ui.graphs.alternatives;

import java.util.ArrayList;

public class AlternativesGraphStrongEdge implements AlternativesGraphEdge{
	
	private ArrayList<String> rules;
	
	
	public AlternativesGraphStrongEdge(ArrayList<String> rules){
		this.rules = rules;
	}
	
	public ArrayList<String> getRules(){
		return rules;
	}
	
	public void setRules(ArrayList<String> rules){
		this.rules = rules;
	}
	
	public void addRule(String rule){
		this.rules.add(rule);
	}
	
	@Override
	public String toString(){
		String toReturn = "{";
		
		int i;
		
		for(i=0; i<rules.size()-1;i++){
			toReturn += rules.get(i)+", ";
		}
		
		toReturn += rules.get(i);
		
		return toReturn +"}";
	}

}

package java_ui.graphs.alternatives;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class AlternativesGraphSimpleVertex implements AlternativesGraphVertex{
	
	private String id;
	private boolean selected;
	
	AlternativesGraphSimpleVertex(String id){
		this.id = id;
		
		checkSelectedState();
	}
	
	@Override
	public String getId(){
		return this.id;
	}
	
	private void checkSelectedState(){
		Query q = new Query("selected_alternative("+ this.id + ")");
		
		this.selected = q.hasSolution();
	}
	
	@Override
	public boolean isSelected(){
		return this.selected;
	}
	
	
	@Override
	public ArrayList<String> getJustificationRulesFor(AlternativesGraphVertex v){
		 ArrayList<String> toReturn = new ArrayList<String>();
		 
		 Query q = new Query("justification_rules("+ this.getId() + ", "+ v.getId() + ", Rules)");
		 
		 while(q.hasNext()){
			 Map<String, Term> s = q.next();
			 
			 Term [] rules = Util.listToTermArray(s.get("Rules"));
			 
			 for(Term r : rules){
				 toReturn.add(r.toString());
			 }
		 }
		 
		 return toReturn;
	}
}

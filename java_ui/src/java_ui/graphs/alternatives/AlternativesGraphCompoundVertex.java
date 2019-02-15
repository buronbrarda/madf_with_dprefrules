package java_ui.graphs.alternatives;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class AlternativesGraphCompoundVertex implements AlternativesGraphVertex{
	
	private ArrayList<String> verteces;
	private boolean selected;
	
	public AlternativesGraphCompoundVertex(ArrayList<String> verteces){
		this.verteces = verteces;
		
		checkSelectedState();
	}
	
	@Override
	public String getId(){
		
		String toReturn = "[";
		
		int i;
		for(i = 0; i < verteces.size()-1; i++){
			toReturn += verteces.get(i) + ", ";
		}
		
		toReturn += verteces.get(i);
		
		return toReturn + "]";
	}
	
	private void checkSelectedState(){
		Query q = new Query("selected_alternative("+ this.verteces.get(0)+ ")");
		
		this.selected = q.hasSolution();
	}
	
	
	
	@Override
	public boolean isSelected() {
		return this.selected;
	}
	
	
	public Collection<String> getVertices(){
		return this.verteces;
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

package java_ui.graphs.alternatives;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import edu.uci.ics.jung.graph.DirectedSparseGraph;

public class AlternativesGraph {
	
	
	private DirectedSparseGraph<AlternativesGraphVertex,AlternativesGraphEdge> graph;
	private HashMap<String,AlternativesGraphVertex> alternativesVertexMap;
	
	public AlternativesGraph(){
		this.graph = new DirectedSparseGraph<AlternativesGraphVertex,AlternativesGraphEdge>();
		this.alternativesVertexMap = new HashMap<String,AlternativesGraphVertex>();
	}
	
	public DirectedSparseGraph<AlternativesGraphVertex,AlternativesGraphEdge> getGraph(){
		return graph;
	}
	
	public void load(){
		
		loadFirstSigthGraph();
		
		//loadVerteces();
		
		//loadStrongEdges();
		
		//loadWeakEdges();
		
	}
	
	
	private void loadVerteces(){
		Query q = new Query("alternative(X)");
		
		Map<String, Term> solution;
		
		while(q.hasNext()){
			solution = q.next();
			buildVertex(solution.get("X").toString());
		}
	}
	
	private void loadStrongEdges(){
		Query q = new Query("justification_rules(X,Y,Rules)");
		
		Map<String, Term> solution;
		
		AlternativesGraphVertex v1, v2;
		ArrayList<String> rules;
		
		while(q.hasNext()){
			solution = q.next();
			
			v1 = alternativesVertexMap.get(solution.get("X").toString());
			v2 = alternativesVertexMap.get(solution.get("Y").toString());
			rules = parseRules(Util.listToTermArray(solution.get("Rules")));
			
			graph.addEdge(new AlternativesGraphStrongEdge(rules), v1, v2);
		}
	}
	
	private void loadWeakEdges(){
		Query q = new Query("weakly_preferred(X,Y)");
		
		Map<String, Term> solution;
		
		AlternativesGraphVertex v1, v2;
		
		while(q.hasNext()){
			solution = q.next();
			
			v1 = alternativesVertexMap.get(solution.get("X").toString());
			v2 = alternativesVertexMap.get(solution.get("Y").toString());
			
			graph.addEdge(new AlternativesGraphWeakEdge(), v1, v2);
		}
	}
	
	private AlternativesGraphVertex buildVertex(String a1) {
		AlternativesGraphVertex v = new AlternativesGraphVertex(a1);
		
		alternativesVertexMap.put(a1, v);
		
		return v;
	}
	
	
	private void loadFirstSigthGraph(){	
		Query q = new Query("equivalent_groups_ranking(Ranking)");
		
		while(q.hasNext()){
			Map<String, Term> s = q.next();
			
			loadFirstSightGraphVerteces(s.get("Ranking"));
			
			loadFirstSightGraphEdges(s.get("Ranking"));	
			
		}
		
	}
	
	
	private void loadFirstSightGraphVerteces(Term ranking){		
		Term [] eq_groups = Util.listToTermArray(ranking);
		
		for(Term eqg : eq_groups){
			
			Term [] groups = Util.listToTermArray(eqg);
			
			for(Term g : groups){
				AlternativesGraphVertex v = buildVertex(listToString(g));
				graph.addVertex(v);
			}
		}
	}
	
	
	private void loadFirstSightGraphEdges(Term ranking) {
		Term [] eq_groups = Util.listToTermArray(ranking);
		
		AlternativesGraphVertex v1, v2;
		
		for(int i = 0; i < eq_groups.length-1; i++){
			
			Term [] groups1 = Util.listToTermArray(eq_groups[i]);
			Term [] groups2 = Util.listToTermArray(eq_groups[i+1]);
			
			for(Term g1 : groups1){
				for(Term g2 : groups2){
					v1 = alternativesVertexMap.get(listToString(g1));
					v2 = alternativesVertexMap.get(listToString(g2));
					
					graph.addEdge(new AlternativesGraphWeakEdge(), v1, v2);
				}
			}
			
		}
		
	}
	
	
	
	private String listToString(Term list){
		String toReturn = "[";
		
		Term [] elements = Util.listToTermArray(list);
		
		int i;
		
		for(i = 0; i<elements.length-1; i++){
			toReturn += elements[i].toString() + ", ";
		}
		
		toReturn += elements[i].toString();
		
		return toReturn + "]";
	}
	
	private ArrayList<String> parseRules(Term [] rules){
		ArrayList<String> toReturn = new ArrayList<String>();
		
		for(Term t : rules){
			toReturn.add(t.toString());
		}
		
		return toReturn;  
		
	}
	
	
	public void clearGraph(){
		this.graph = new DirectedSparseGraph<AlternativesGraphVertex,AlternativesGraphEdge>();
		this.alternativesVertexMap = new HashMap<String,AlternativesGraphVertex>();
	}
	
	
	
}

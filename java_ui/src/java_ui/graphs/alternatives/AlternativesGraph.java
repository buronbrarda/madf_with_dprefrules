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
		
		loadVerteces();
		
		loadStrongEdges();
		
		loadWeakEdges();
		
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

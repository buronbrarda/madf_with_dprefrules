package java_ui.graphs.alternatives;

import java.util.ArrayList;
import java.util.Collection;
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
	
	/*
	private void loadVerteces(){
		Query q = new Query("alternative(X)");
		
		Map<String, Term> solution;
		
		while(q.hasNext()){
			solution = q.next();
			buildVertex(solution.get("X").toString());
		}
	}
	*/
	
	/*
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
	*/
	
	/*
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
	
	*/
	
	private AlternativesGraphVertex buildVertex(Term [] verteces_group) {
		
		AlternativesGraphVertex toReturn;
		
		if(verteces_group.length == 1){
			
			toReturn = new AlternativesGraphSimpleVertex(verteces_group[0].toString());
		
			alternativesVertexMap.put(toReturn.getId(), toReturn);
			
		}else{
			
			toReturn = new AlternativesGraphCompoundVertex(termArrayToStringArray(verteces_group));
			
			alternativesVertexMap.put(toReturn.getId(), toReturn);
			
		}
		
		return toReturn;
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
				AlternativesGraphVertex v = buildVertex(Util.listToTermArray(g));
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
					
					Term [] array1 = Util.listToTermArray(g1);
					Term [] array2 = Util.listToTermArray(g2);
					
					String id1 = array1.length == 1 ? array1[0].toString() : listToString(g1);
					String id2 = array2.length == 1 ? array2[0].toString() : listToString(g2);
					
					v1 = alternativesVertexMap.get(id1);
					v2 = alternativesVertexMap.get(id2);					
					
					graph.addEdge(new AlternativesGraphStrongEdge(v1.getJustificationRulesFor(v2)), v1, v2);
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
	
	
	private ArrayList<String> termArrayToStringArray(Term [] elements){
		ArrayList<String> toReturn = new ArrayList<String>();
		
		for(Term e : elements){
			toReturn.add(e.toString());
		}
		
		return toReturn;
	}
	
	/*
	private ArrayList<String> parseRules(Term [] rules){
		ArrayList<String> toReturn = new ArrayList<String>();
		
		for(Term t : rules){
			toReturn.add(t.toString());
		}
		
		return toReturn;  
		
	}
	*/
	
	public void clearGraph(){
		this.graph = new DirectedSparseGraph<AlternativesGraphVertex,AlternativesGraphEdge>();
		this.alternativesVertexMap = new HashMap<String,AlternativesGraphVertex>();
	}

	public void expand(AlternativesGraphCompoundVertex v) {
		Collection<AlternativesGraphEdge> in = this.graph.getInEdges(v);
		Collection<AlternativesGraphEdge> out = this.graph.getOutEdges(v);
		
		ArrayList<AlternativesGraphVertex> predecessors = new ArrayList<AlternativesGraphVertex>();
		ArrayList<AlternativesGraphVertex> successors = new ArrayList<AlternativesGraphVertex>();
		
		for(AlternativesGraphEdge e : in){
			predecessors.add(this.graph.getSource(e));
		}
		
		for(AlternativesGraphEdge e : out){
			successors.add(this.graph.getDest(e));
		}
		
		ArrayList<AlternativesGraphSimpleVertex> expandedVertices = new ArrayList<AlternativesGraphSimpleVertex>();
		
		this.graph.removeVertex(v);
		
		for(String id : v.getVertices()){
			
			AlternativesGraphSimpleVertex simpleVertex = new AlternativesGraphSimpleVertex(id);
						
			this.graph.addVertex(simpleVertex);
			
			expandedVertices.add(simpleVertex);
			
			for(AlternativesGraphVertex p : predecessors){
				this.graph.addEdge(new AlternativesGraphStrongEdge(p.getJustificationRulesFor(simpleVertex)), p, simpleVertex);
			}
			
			for(AlternativesGraphVertex s : successors){
				this.graph.addEdge(new AlternativesGraphStrongEdge(simpleVertex.getJustificationRulesFor(s)), simpleVertex, s);
			}
		}
		
		this.loadExpandedEdges(expandedVertices);
	}
	
	
	private void loadExpandedEdges(ArrayList<AlternativesGraphSimpleVertex> expandedVertices) {
		
		
		for(AlternativesGraphSimpleVertex v1 : expandedVertices){
			for(AlternativesGraphSimpleVertex v2 : expandedVertices){
				
				if(v1 != v2){
					
					Query q = new Query("explicitly_preferred("+v1.getId()+","+v2.getId()+")");
					
					if(q.hasSolution()){
						
						this.graph.addEdge(new AlternativesGraphStrongEdge(v1.getJustificationRulesFor(v2)), v1, v2);
					}
					
				}
				
			}
		}
		
	}
	
	
	
}

package java_ui.graphs.alternatives;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import java_ui.graphs.alternatives.lattice.JungLatticeGraph;

public class AlternativesGraph {
	
	
	private JungLatticeGraph<AlternativesGraphVertex,AlternativesGraphEdge> graph;
	private HashMap<String,AlternativesGraphVertex> alternativesVertexMap;
	
	public AlternativesGraph(){
		this.graph = new JungLatticeGraph<AlternativesGraphVertex,AlternativesGraphEdge>();
		this.alternativesVertexMap = new HashMap<String,AlternativesGraphVertex>();
	}
	
	public JungLatticeGraph<AlternativesGraphVertex, AlternativesGraphEdge> getGraph(){
		return graph;
	}
	
	public void load(){
		
		loadFirstSigthGraph("null");
		
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
			
			String id = verteces_group[0].toString();
			toReturn = alternativesVertexMap.get(id);
			
			if(toReturn == null) {
				toReturn = new AlternativesGraphSimpleVertex(id);
				alternativesVertexMap.put(toReturn.getId(), toReturn);
			}
			
			
		}else{
			
			
			toReturn = new AlternativesGraphCompoundVertex(termArrayToStringArray(verteces_group));
			AlternativesGraphVertex aux = alternativesVertexMap.get(toReturn.getId());
			
			if(aux == null) {
				alternativesVertexMap.put(toReturn.getId(), toReturn);
			}
			else {
				toReturn = aux;
			}
			
		}
		
		return toReturn;
	}

	private void loadFirstSigthGraph(String parent){
		Query q = new Query("ranking_parent("+parent+",Kid)");
		
		LinkedList<Term> kids = new LinkedList<Term>();

		for(Map<String, Term> solution : q) {
			kids.add(solution.get("Kid"));
		}
			
		loadFirstSightGraphVerteces(kids);
		
		if(!parent.toString().equals("null")) {
			loadFirstSightGraphEdges(Util.textToTerm(parent),kids);
		}
		
		for(Term k : kids) {
			loadFirstSigthGraph(listToString(k));
		}

		
	}
	
	
	private void loadFirstSightGraphVerteces(List<Term> groups){		
		for(Term g : groups){
			AlternativesGraphVertex v = buildVertex(Util.listToTermArray(g));
			graph.addVertex(v);
		}
	}
	
	
	private void loadFirstSightGraphEdges(Term parent, List<Term> kids) {
		AlternativesGraphVertex v1, v2;
		Term [] parentArray = Util.listToTermArray(parent);
		
		for(Term group : kids){		
			
			Term [] kidArray = Util.listToTermArray(group);
			
			String id1 = parentArray.length == 1 ? parentArray[0].toString() : listToString(parent);
			String id2 = kidArray.length == 1 ? kidArray[0].toString() : listToString(group);
			
			v1 = alternativesVertexMap.get(id1);
			v2 = alternativesVertexMap.get(id2);
			
			if(graph.findEdge(v1, v2) == null) {
				ArrayList <String> list = new ArrayList<String>(); list.add("r*");
				graph.addEdge(new AlternativesGraphStrongEdge(list), v1, v2);
				//graph.addEdge(new AlternativesGraphStrongEdge(v1.getJustificationRulesFor(v2)), v1, v2);
				
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
		this.graph = new JungLatticeGraph<AlternativesGraphVertex,AlternativesGraphEdge>();
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
		
		ArrayList<AlternativesGraphVertex> expandedVertices = new ArrayList<AlternativesGraphVertex>();
		
		this.graph.removeVertex(v);
		
		for(String id : v.getVertices()){
			
			AlternativesGraphSimpleVertex simpleVertex = new AlternativesGraphSimpleVertex(id);
						
			this.graph.addVertex(simpleVertex);
			
			expandedVertices.add(simpleVertex);
			
			for(AlternativesGraphVertex p : predecessors){
				ArrayList <String> list = new ArrayList<String>(); list.add("r*");
				this.graph.addEdge(new AlternativesGraphStrongEdge(list), p, simpleVertex);
				//this.graph.addEdge(new AlternativesGraphStrongEdge(p.getJustificationRulesFor(simpleVertex)), p, simpleVertex);
			}
			
			for(AlternativesGraphVertex s : successors){
				this.graph.addEdge(new AlternativesGraphStrongEdge(simpleVertex.getJustificationRulesFor(s)), simpleVertex, s);
			}
		}
		
		this.loadExpandedEdges(expandedVertices);
	}
	
	
	private void loadExpandedEdges(ArrayList<AlternativesGraphVertex> expandedVertices) {
		
		
		for(AlternativesGraphVertex v1 : expandedVertices){
			for(AlternativesGraphVertex v2 : expandedVertices){
				
				if(v1 != v2 && v1 instanceof AlternativesGraphSimpleVertex && v2 instanceof AlternativesGraphSimpleVertex){
					
					Query q = new Query("explicitly_preferred("+v1.getId()+","+v2.getId()+")");
					
					if(q.hasSolution()){
						
						ArrayList <String> list = new ArrayList<String>(); list.add("r*");
						this.graph.addEdge(new AlternativesGraphStrongEdge(list), v1, v2);
						//this.graph.addEdge(new AlternativesGraphStrongEdge(v1.getJustificationRulesFor(v2)), v1, v2);
						
						q.close();
					}
					
				}
				
			}
		}
		
	}

	public void addExtraEdgesBetween(Set<AlternativesGraphVertex> picked) {
		
		loadExpandedEdges(new ArrayList<AlternativesGraphVertex>(picked));
		
	}
	
	
	
}

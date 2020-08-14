package java_ui.arguments.dialectical_trees;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import java_ui.arguments.Argument;

public class ArgumentsGraph {
	
	
	private DirectedSparseGraph<Argument,DTreeEdge> graph;
	private ArrayList<Argument> arguments;
	
	public ArgumentsGraph(ArrayList<Argument> arguments){
		this.graph = new DirectedSparseGraph<Argument,DTreeEdge>();
		this.arguments = arguments;
	}
	
	public ArgumentsGraph() {
		this.graph = new DirectedSparseGraph<Argument,DTreeEdge>();
		this.arguments = null;
	}

	public DirectedSparseGraph<Argument,DTreeEdge> getGraph(){
		return graph;
	}
	
	public void load(){
		
		if(this.arguments == null) {
			loadFullGraph();
		}
		else {
			for(Argument a : arguments) {
				this.graph.addVertex(a);
			}
		}
		
		for(Argument v1 : this.graph.getVertices()) {
			for(Argument v2 : this.graph.getVertices()) {
				Query q = new Query("defeats("+v1.getId()+","+v2.getId()+")");
				if(q.hasNext()) {
					q.next();
					this.graph.addEdge(new DTreeEdge(), v1, v2);
				}
			}
		}
		
	}
	
	private void loadFullGraph(){
		ArrayList<String> ids = new ArrayList<String>();
		for(Map<String, Term> s : new Query("argument(ArgId,_,_)")) {
			ids.add(s.get("ArgId").toString());
		}
		
		for(String id : ids) {
			this.graph.addVertex(new Argument(id));
		}
	}

	public void clearGraph() {
		for(Argument v : this.graph.getVertices()) {
			this.graph.removeVertex(v);
		}
		
	}
	
}

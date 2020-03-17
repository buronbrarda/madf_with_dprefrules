package java_ui.arguments.dialectical_trees;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import java_ui.arguments.Argument;

public class ArgumentsGraph {
	
	
	private DirectedSparseGraph<Argument,DTreeEdge> graph;
	
	public ArgumentsGraph(){
		this.graph = new DirectedSparseGraph<Argument,DTreeEdge>();
	}
	
	public DirectedSparseGraph<Argument,DTreeEdge> getGraph(){
		return graph;
	}
	
	public void load(){
		
		ArrayList<String> ids = new ArrayList<String>();
		for(Map<String, Term> s : new Query("argument(ArgId,_,_)")) {
			ids.add(s.get("ArgId").toString());
		}
		
		for(String id : ids) {
			this.graph.addVertex(new Argument(id));
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

	public void clearGraph() {
		for(Argument v : this.graph.getVertices()) {
			this.graph.removeVertex(v);
		}
		
	}
	
}

package java_ui.arguments.dialectical_trees;

import java.util.ArrayList;
import edu.uci.ics.jung.graph.DelegateForest;
import edu.uci.ics.jung.graph.Forest;
import edu.uci.ics.jung.graph.Tree;

public class DeltaExplanation extends DelegateForest<DTreeNode, DTreeEdge> implements Forest<DTreeNode,DTreeEdge>{
	
	public DeltaExplanation(ArrayList<DialecticalTree> dtrees){
		super();
		
		for(DialecticalTree t : dtrees){
			this.addTree(t);
		};
	}
	
	/*
	
	private Tree<DTreeNode, DTreeEdge> findTree(DTreeNode v){
		for(Tree<DTreeNode, DTreeEdge> t : this.dtrees){
			if(t.containsVertex(v)){
				return t;
			}
		}
		return null;
	}
	
	private Tree<DTreeNode, DTreeEdge> findTree(DTreeEdge e){
		for(Tree<DTreeNode, DTreeEdge> t : this.dtrees){
			if(t.containsEdge(e)){
				return t;
			}
		}
		return null;
	}
	
	
	@Override
	public boolean addEdge(DTreeEdge e, DTreeNode v1, DTreeNode v2) {
		boolean succed = false;
		
		Tree<DTreeNode, DTreeEdge> t1 = findTree(v1);
		Tree<DTreeNode, DTreeEdge> t2 = findTree(v2);
		
		succed = t1 != null && t1 == t2;
		
		if(succed){
			succed = t1.addEdge(e, v1, v2);
		}
		
		
		return succed;
	}

	@Override
	public boolean addEdge(DTreeEdge e, DTreeNode v1, DTreeNode v2, EdgeType edgeType) {
		boolean succed = false;
		
		if(edgeType == EdgeType.DIRECTED){
			succed = this.addEdge(e, v1, v2);
		}
		
		return succed;
	}

	@Override
	public DTreeNode getDest(DTreeEdge directed_edge) {
		DTreeNode toReturn = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(directed_edge);
		
		if(t != null){
			toReturn = t.getDest(directed_edge);
		}
		
		return toReturn;
	}

	@Override
	public Pair<DTreeNode> getEndpoints(DTreeEdge edge) {
		Pair<DTreeNode> toReturn = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(edge);
		
		if(t != null){
			toReturn = t.getEndpoints(edge);
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeEdge> getInEdges(DTreeNode vertex) {
		Collection<DTreeEdge> edges = null;
				
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			edges = t.getInEdges(vertex);
		}
		
		return edges;
	}

	@Override
	public DTreeNode getOpposite(DTreeNode vertex, DTreeEdge edge) {
		DTreeNode toReturn = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.getOpposite(vertex, edge);
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeEdge> getOutEdges(DTreeNode vertex) {
		Collection<DTreeEdge> edges = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			edges = t.getOutEdges(vertex);
		}
		
		return edges;
	}

	@Override
	public int getPredecessorCount(DTreeNode vertex) {
		int toReturn = 0;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.getPredecessorCount(vertex);
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeNode> getPredecessors(DTreeNode vertex) {
		Collection<DTreeNode> vertices = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			vertices = t.getPredecessors(vertex);
		}
		
		return vertices;
	}

	@Override
	public DTreeNode getSource(DTreeEdge directed_edge) {
		DTreeNode toReturn = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(directed_edge);
		
		if(t != null){
			toReturn = t.getSource(directed_edge);
		}
		
		return toReturn;
	}

	@Override
	public int getSuccessorCount(DTreeNode vertex) {
		int toReturn = 0;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.getSuccessorCount(vertex);
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeNode> getSuccessors(DTreeNode vertex) {
		Collection<DTreeNode> vertices = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			vertices = t.getSuccessors(vertex);
		}
		
		return vertices;
	}

	@Override
	public int inDegree(DTreeNode vertex) {
		int toReturn = 0;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.inDegree(vertex);
		}
		
		return toReturn;
	}

	@Override
	public boolean isDest(DTreeNode vertex, DTreeEdge edge) {
		boolean toReturn = false;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.isDest(vertex,edge);
		}
		
		return toReturn;
	}

	@Override
	public boolean isPredecessor(DTreeNode v1, DTreeNode v2) {
		boolean toReturn = false;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(v1);
		
		if(t != null){
			toReturn = t.isPredecessor(v1,v2);
		}
		
		return toReturn;
	}

	@Override
	public boolean isSource(DTreeNode vertex, DTreeEdge edge) {
		boolean toReturn = false;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.isSource(vertex,edge);
		}
		
		return toReturn;
	}

	@Override
	public boolean isSuccessor(DTreeNode v1, DTreeNode v2) {
		boolean toReturn = false;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(v1);
		
		if(t != null){
			toReturn = t.isSuccessor(v1,v2);
		}
		
		return toReturn;
	}

	@Override
	public int outDegree(DTreeNode vertex) {
		int toReturn = 0;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			toReturn = t.inDegree(vertex);
		}
		
		return toReturn;
	}

	@Override
	public boolean addEdge(DTreeEdge edge, Collection<? extends DTreeNode> vertices) {
		
		boolean succed = vertices.size() == 2;
		
		DTreeNode [] array= vertices.toArray(new DTreeNode[2]);
		
		if(succed){
			succed = this.addEdge(edge, array[0], array[1]);
		}
		
		return false;
	}

	@Override
	public boolean addEdge(DTreeEdge edge, Collection<? extends DTreeNode> vertices, EdgeType edge_type) {
		boolean succed = edge_type == EdgeType.DIRECTED;
		
		if(succed){
			succed = this.addEdge(edge,vertices);
		}
		
		return false;
	}

	@Override
	public boolean addVertex(DTreeNode vertex) {
		boolean succed = findTree(vertex) == null;
		
		if(succed){
			dtrees.add(new DialecticalTree(vertex));
		}
		
		return succed;
	}

	@Override
	public boolean containsEdge(DTreeEdge edge) {
		return findTree(edge) == null;
	}

	@Override
	public boolean containsVertex(DTreeNode vertex) {
		return findTree(vertex) == null;
	}

	@Override
	public int degree(DTreeNode vertex) {
		return inDegree(vertex)+outDegree(vertex);
	}

	@Override
	public DTreeEdge findEdge(DTreeNode v1, DTreeNode v2) {
		DTreeEdge toReturn = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(v1);
		
		if(t!=null){
			toReturn = t.findEdge(v1, v2);
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeEdge> findEdgeSet(DTreeNode v1, DTreeNode v2) {
		DTreeEdge edge = this.findEdge(v1,v2);
		
		Collection<DTreeEdge> toReturn = new ArrayList<DTreeEdge>();
		
		if(edge != null){
			toReturn.add(edge);
		}
		
		return toReturn;
	}

	@Override
	public EdgeType getDefaultEdgeType() {
		return EdgeType.DIRECTED;
	}

	@Override
	public int getEdgeCount() {
		
		int toReturn = 0;
		
		for( Tree<DTreeNode, DTreeEdge> t : this.dtrees){
			toReturn += t.getEdgeCount();
		}
		
		return toReturn;
	}

	@Override
	public int getEdgeCount(EdgeType edge_type) {
		return this.getEdgeCount();
	}

	@Override
	public EdgeType getEdgeType(DTreeEdge edge) {
		return EdgeType.DIRECTED;
	}

	@Override
	public Collection<DTreeEdge> getEdges() {
		ArrayList<DTreeEdge> toReturn = new ArrayList<DTreeEdge>();
		
		for( Tree<DTreeNode, DTreeEdge> t : this.dtrees){
			toReturn.addAll(t.getEdges());
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeEdge> getEdges(EdgeType edge_type) {
		return this.getEdges();
	}

	@Override
	public int getIncidentCount(DTreeEdge edge) {
		return 2;
	}

	@Override
	public Collection<DTreeEdge> getIncidentEdges(DTreeNode vertex) {
		Collection<DTreeEdge> edges = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			edges = t.getIncidentEdges(vertex);
		}
		
		return edges;
	}

	@Override
	public Collection<DTreeNode> getIncidentVertices(DTreeEdge edge) {
		Collection<DTreeNode> vertices = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(edge);
		
		if(t != null){
			vertices = t.getIncidentVertices(edge);
		}
		
		return vertices;
	}

	@Override
	public int getNeighborCount(DTreeNode vertex) {
		int count = 0;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			count = t.getNeighborCount(vertex);
		}
		
		return count;
	}

	@Override
	public Collection<DTreeNode> getNeighbors(DTreeNode vertex) {
		Collection<DTreeNode> vertices = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			vertices = t.getNeighbors(vertex);
		}
		
		return vertices;
	}

	@Override
	public int getVertexCount() {
		int toReturn = 0;
		
		for( Tree<DTreeNode, DTreeEdge> t : this.dtrees){
			toReturn += t.getVertexCount();
		}
		
		return toReturn;
	}

	@Override
	public Collection<DTreeNode> getVertices() {
		ArrayList<DTreeNode> toReturn = new ArrayList<DTreeNode>();
		
		for( Tree<DTreeNode, DTreeEdge> t : this.dtrees){
			toReturn.addAll(t.getVertices());
		}
		
		return toReturn;
	}

	@Override
	public boolean isIncident(DTreeNode vertex, DTreeEdge edge) {
		boolean succed = false;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			succed = t.isIncident(vertex,edge);
		}
		
		return succed;
	}

	@Override
	public boolean isNeighbor(DTreeNode v1, DTreeNode v2) {
		boolean succed = false;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(v1);
		
		if(t != null){
			succed = t.isNeighbor(v1,v2);
		}
		
		return succed;
	}

	@Override
	public boolean removeEdge(DTreeEdge edge) {
		return findTree(edge).removeEdge(edge);
	}

	@Override
	public boolean removeVertex(DTreeNode vertex) {
		return findTree(vertex).removeVertex(vertex);
	}

	@Override
	public int getChildCount(DTreeNode vertex) {
		return vertex.getChildren().size();
	}

	@Override
	public Collection<DTreeEdge> getChildEdges(DTreeNode vertex) {
		Collection<DTreeEdge> edges = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			edges = t.getChildEdges(vertex);
		}
		
		return edges;
	}

	@Override
	public Collection<DTreeNode> getChildren(DTreeNode vertex) {
		return vertex.getChildren();
	}

	@Override
	public DTreeNode getParent(DTreeNode vertex) {
		return vertex.getParent();
	}

	@Override
	public DTreeEdge getParentEdge(DTreeNode vertex) {
		DTreeEdge edges = null;
		
		Tree<DTreeNode, DTreeEdge> t = findTree(vertex);
		
		if(t != null){
			edges = t.getParentEdge(vertex);
		}
		
		return edges;
	}

	@Override
	public Collection<Tree<DTreeNode, DTreeEdge>> getTrees() {
		return this.dtrees;
	}
	*/

	public void clear() {
		for(Tree<DTreeNode, DTreeEdge> t : this.getTrees()){
			((DialecticalTree)t).clear();
		}
		
		//dtrees.clear();
	}
	
}

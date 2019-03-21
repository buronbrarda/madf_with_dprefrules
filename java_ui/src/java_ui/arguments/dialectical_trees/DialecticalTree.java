package java_ui.arguments.dialectical_trees;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import edu.uci.ics.jung.graph.DirectedSparseGraph;
import edu.uci.ics.jung.graph.Tree;
import java_ui.arguments.Argument;

public class DialecticalTree extends DirectedSparseGraph<DTreeNode, DTreeEdge> implements Tree<DTreeNode, DTreeEdge>{
	
	private int height;
	private DTreeNode root;
	
	
	public DialecticalTree(){
		super();
	}
	
	public DialecticalTree(DTreeNode root){
		this.root = root;
		root.setParent(null);
		this.height = 0;
	}
	
	public void load(Argument arg){
		Query q = new Query("dtree_node(Node_Id,null,Children,"+arg.getId()+",Status)");
		
		if(q.hasNext()){
			Map<String, Term> s = q.next();
			
			this.root = new DTreeNode(s.get("Node_Id").toString(), arg, s.get("Status").toString());
			this.root.setParent(null);
			
			this.addVertex(this.root);
			
			buildChildren(this.root, Util.listToTermArray(s.get("Children")));
		}
	}
	
	
	private void buildChildren(DTreeNode node, Term [] children){
		
		if(children.length > 0){
			
			Query q;
			
			for(Term child : children){
				
				q = new Query("dtree_node("+child.toString()+","+node.getId()+",Children,Arg_Id,Status)");
				
				if(q.hasNext()){
					Map<String, Term> s = q.next();
					
					Argument argument = new Argument(s.get("Arg_Id").toString());
					
					DTreeNode childNode = new DTreeNode(child.toString(), argument, s.get("Status").toString());
					
					this.addEdge(new DTreeEdge(), childNode, node);
					
					buildChildren(childNode, Util.listToTermArray(s.get("Children")));
				}
				
			}
			
		}
	}
	
	
	@Override
	public boolean addEdge(DTreeEdge e, DTreeNode v1, DTreeNode v2) {
		boolean succed = false;
		
		if(super.containsVertex(v2) && !super.containsVertex(v1) && super.addEdge(e, v1, v2)){
			
			succed = true;
			
			v1.getChildren().add(v2);
			v2.setParent(v1);
			
			this.height = Math.max(this.height, v2.getDepth());
		}
		
		return succed;
	}
	
	@Override
	public int getChildCount(DTreeNode vertex) {
		return vertex.getChildren().size();
	}

	@Override
	public Collection<DTreeEdge> getChildEdges(DTreeNode vertex) {
		return this.getOutEdges(vertex);
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
		return this.getInEdges(vertex).toArray(new DTreeEdge[1])[0];
	}

	@Override
	public Collection<Tree<DTreeNode, DTreeEdge>> getTrees() {
		ArrayList<Tree<DTreeNode, DTreeEdge>> trees = new ArrayList<Tree<DTreeNode, DTreeEdge>>();
		
		trees.add(this);
		
		return trees;
	}

	@Override
	public int getDepth(DTreeNode vertex) {
		return vertex.getDepth();
	}

	@Override
	public int getHeight() {
		return this.height;
	}

	@Override
	public DTreeNode getRoot() {
		return this.root;
	}

	public void clear() {
		
		Collection<DTreeNode> vertices = this.getVertices();
		
		for(DTreeNode v : vertices){
			this.removeVertex(v);
		}
		
		this.root = null;
		
	}

	

}

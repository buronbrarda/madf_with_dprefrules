package java_ui.arguments.dialectical_trees;

import java.util.ArrayList;

import java_ui.arguments.Argument;

public class DTreeNode {
	
	public enum Status {DEFEATED, UNDEFEATED};
	
	private String id;
	
	private DTreeNode parent;
	private ArrayList<DTreeNode> children;
	
	private Argument argument;
	
	private Status status;
	
	
	public DTreeNode(String id, Argument argument, String status){
		this.id = id;
		
		this.argument = argument;
		
		this.children = new ArrayList<DTreeNode>();
		
		if(status.equals("'U'")){
			this.status = Status.UNDEFEATED;
		}else{
			if(status.equals("'D'")){
				this.status = Status.DEFEATED;
			}
		}
	}
	
	
	public DTreeNode getParent(){
		return this.parent;
	}
	
	
	public ArrayList<DTreeNode> getChildren(){
		return this.children;
	}
	
	public Argument getArgument(){
		return this.argument;
	}
	
	
	
	public void setParent(DTreeNode p){
		this.parent = p;
	}
	
	
	public void setChildren(ArrayList<DTreeNode> children){
		this.children = children;
	}
	
	
	public void setArgument(Argument a){
		this.argument = a;
	}
	
	
	public String getArgumentClaim(){
		return this.argument.getClaim();
	}
	
	
	public String getArgumentRules(){
		String toReturn = "{";
		
		int i;
		for(i=0; i < this.argument.getRules().size()-1; i++){
			toReturn += this.argument.getRules().get(i)+", ";
		}
		
		toReturn += this.argument.getRules().get(i);
		
		return toReturn + "}";
	}
	
	
	public int getDepth(){
		
		if(this.parent == null){
			return 0;
		}
		else{
			return 1+this.parent.getDepth();
		}
	}
	
	
	public String getId(){
		return this.id;
	}
	
	public Status getStatus(){
		return this.status;
	}

}

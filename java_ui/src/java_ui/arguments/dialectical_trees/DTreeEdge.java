package java_ui.arguments.dialectical_trees;

public class DTreeEdge {

	private int id;
	
	public DTreeEdge(){
		this.id = this.hashCode();
	}
	
	public int getId(){
		return this.id;
	}
}

package java_ui.arguments.dialectical_trees;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Shape;
import java.util.ArrayList;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.DAGLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;
import edu.uci.ics.jung.visualization.decorators.EllipseVertexShapeTransformer;
import java_ui.arguments.Argument;
import java_ui.arguments.dialectical_trees.DTreeNode.Status;

public class DeltaExplanationPanel extends JPanel {
	
	private DeltaExplanation graph;
	private DAGLayout<DTreeNode, DTreeEdge> layout;
	private VisualizationViewer<DTreeNode, DTreeEdge> vv;
	private DefaultModalGraphMouse<DTreeNode, DTreeEdge> mouse;
	
	public DeltaExplanationPanel(){
		setLayout(new BorderLayout());
		
		this.mouse = new DefaultModalGraphMouse<DTreeNode, DTreeEdge>();
		
	}
	
	
	private void initVisualizerViewer(){
		this.vv = new VisualizationViewer<DTreeNode, DTreeEdge>(this.layout);
		
		
		this.vv.setGraphMouse(this.mouse);
		
		
		JMenu menu = this.mouse.getModeMenu();
		menu.setText("MOUSE MODE");
		menu.setPreferredSize(new Dimension(200, 20));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(menu);
		
		this.add(menuBar, BorderLayout.NORTH);
		
		this.mouse.setMode(Mode.PICKING);
		
		this.vv.getRenderContext().setVertexFillPaintTransformer(new Transformer<DTreeNode, Paint>() {

			@Override
			public Paint transform(DTreeNode v) {
				if(v.getStatus() == Status.DEFEATED){
					return Color.RED;
				}
				else{
					if(v.getStatus() == Status.UNDEFEATED){
						return Color.GREEN;
					}
					else{
						return null;
					}
				}
				
			}
		});
		
		
		this.vv.getRenderContext().setEdgeLabelTransformer(new Transformer<DTreeEdge, String>(){
				
			@Override
			public String transform(DTreeEdge edge){
				return "";
			}
			
		});
	
	
		this.vv.getRenderContext().setVertexLabelTransformer(new Transformer<DTreeNode, String>(){

			@Override
			public String transform(DTreeNode vertex) {
				
				String label = "";
				
				if(vertex != null){
					label += vertex.getArgumentClaim()+"//"+vertex.getArgumentRules();
				}
				
				return label;
			}
			
		});
		
		this.vv.getRenderContext().setVertexShapeTransformer(new DTreeNodeShapeTransformer());
	}
	
	public void loadGraph(ArrayList<Argument> arguments){
		
		ArrayList<DialecticalTree> dtrees = new ArrayList<DialecticalTree>();
		
		for(Argument arg : arguments){
			DialecticalTree t = new DialecticalTree();
			t.load(arg);
			dtrees.add(t);
		}
		
		this.graph = new DeltaExplanation(dtrees);
		
		this.layout = new  DAGLayout<DTreeNode, DTreeEdge>(this.graph);
		this.layout.setForceMultiplier(0);
		
		if(this.vv != null){
			this.remove(this.vv);
		}
			
		initVisualizerViewer();
		
		this.add(this.vv, BorderLayout.CENTER);
	}
	
	public void clearGraph(){
		this.graph.clear();
		this.vv.repaint();
	}
	
	
	private class DTreeNodeShapeTransformer extends EllipseVertexShapeTransformer<DTreeNode>{
		
		DTreeNodeShapeTransformer() {
            setSizeTransformer(new Transformer<DTreeNode, Integer>() {
            	
            	public Integer transform(DTreeNode v){
            		return 30;
            	}
			});
        }
		
		public Shape transform(DTreeNode v) {
            return factory.getRegularPolygon(v, 3);
        }
	}
}

package java_ui.arguments.dialectical_trees;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.DAGLayout;
import edu.uci.ics.jung.algorithms.layout.TreeLayout;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;
import edu.uci.ics.jung.visualization.decorators.DirectionalEdgeArrowTransformer;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.decorators.EllipseVertexShapeTransformer;
import java_ui.arguments.Argument;
import java_ui.arguments.dialectical_trees.DTreeNode.Status;
import java_ui.graphs.alternatives.lattice.Utils;

public class DeltaExplanationPanel extends JPanel {
	
	private DeltaExplanation graph;
	private TreeLayout<DTreeNode, DTreeEdge> layout;
	private VisualizationViewer<DTreeNode, DTreeEdge> vv;
	private DefaultModalGraphMouse<DTreeNode, DTreeEdge> mouse;
	
	private ArgumentsGraphPanel argumentsPanel = null;
	private String selectedArgId;
	
	private final Color DefeatedColor = new Color(250, 213, 62);
	private final Color UndefeatedColor = new Color(62, 202, 250);
	
	private final Color DefeatedSelectedColor = new Color(252, 157, 25);
	private final Color UndefeatedSelectedColor = new Color(43, 152, 249);
	
	
	
	public DeltaExplanationPanel(){
		setLayout(new BorderLayout());
		
		this.mouse = new DefaultModalGraphMouse<DTreeNode, DTreeEdge>();
		
	}
	
	
	private void initVisualizerViewer(){
		this.selectedArgId = "";
		
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
					return v.getArgument().getId().equals(selectedArgId) ? DefeatedSelectedColor : DefeatedColor;
				}
				else{
					if(v.getStatus() == Status.UNDEFEATED){
						return v.getArgument().getId().equals(selectedArgId) ? UndefeatedSelectedColor : UndefeatedColor;
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
		
		this.vv.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line<DTreeNode,DTreeEdge>());
		
		this.vv.getRenderContext().setEdgeArrowTransformer(new DirectionalEdgeArrowTransformer<DTreeNode,DTreeEdge>(0, 0, 0));
		
		this.vv.getRenderContext().setVertexLabelTransformer(new Transformer<DTreeNode, String>(){

			@Override
			public String transform(DTreeNode vertex) {
				
				String label = "";
				
				if(vertex != null){
					label += "A"+vertex.getArgument().getId();
				}
				
				return label;
			}
			
		});
		
		this.vv.getRenderContext().setVertexShapeTransformer(new DTreeNodeShapeTransformer());
		
		this.vv.getPickedVertexState().addItemListener(new ItemListener() {
			
			@Override
			public void itemStateChanged(ItemEvent e) {
				DTreeNode node = (DTreeNode) e.getItem();
				
				if(vv.getPickedVertexState().isPicked(node)) {
					String argId = node.getArgument().getId();
					setSelectedArgument(argId);
					if(argumentsPanel != null)
						argumentsPanel.setSelectedArgument(argId);
				}
				
				if(vv.getPickedVertexState().getPicked().isEmpty()) {
					setSelectedArgument("");
					if(argumentsPanel != null)
						argumentsPanel.setSelectedArgument("");
				}
				
				
			}
		});
		
		this.vv.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent arg0) {
				super.componentResized(arg0);
				Utils.fitGraph(vv, layout);
			}});
	}
	
	


	public void loadGraph(ArrayList<Argument> arguments){
		
		ArrayList<DialecticalTree> dtrees = new ArrayList<DialecticalTree>();
		
		for(Argument arg : arguments){
			DialecticalTree t = new DialecticalTree();
			t.load(arg);
			dtrees.add(t);
		}
		
		this.graph = new DeltaExplanation(dtrees);
		
		//this.layout = new  DAGLayout<DTreeNode, DTreeEdge>(this.graph);
		//this.layout.setForceMultiplier(0);
		
		this.layout = new  TreeLayout<DTreeNode, DTreeEdge>(this.graph);
		
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
            		return v.getArgument().getId().equals(selectedArgId) ? 40 : 30;
            	}
			});
        }
		
		public Shape transform(DTreeNode v) {
            return factory.getRegularPolygon(v, 3);
        }
	}
	
	public void setSelectedArgument(String id) {
		this.selectedArgId = id;
		this.vv.repaint();
	}
	
	public void setArgumentsPanel(ArgumentsGraphPanel panel) {
		this.argumentsPanel = panel;
	}
}

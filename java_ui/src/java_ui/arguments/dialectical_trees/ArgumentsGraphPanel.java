package java_ui.arguments.dialectical_trees;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.ArrayList;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.ISOMLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;
import edu.uci.ics.jung.visualization.decorators.EllipseVertexShapeTransformer;
import java_ui.arguments.Argument;
import java_ui.graphs.alternatives.lattice.Utils;

public class ArgumentsGraphPanel extends JPanel {
	
	private ArgumentsGraph graph;
	private ISOMLayout<Argument, DTreeEdge> layout;
	private VisualizationViewer<Argument, DTreeEdge> vv;
	private DefaultModalGraphMouse<Argument, DTreeEdge> mouse;
	
	private final Color acceptedArgumentColor = new Color(31, 221, 19);
	private final Color rejectedArgumentColor = new Color(243, 12, 12);
	
	
	public ArgumentsGraphPanel(){
		this.graph = new ArgumentsGraph();
		setLayout(new BorderLayout());
		this.mouse = new DefaultModalGraphMouse<Argument, DTreeEdge>();
		
	}
	
	public ArgumentsGraphPanel(ArrayList<Argument> arguments){
		this.graph = new ArgumentsGraph(arguments);
		setLayout(new BorderLayout());
		this.mouse = new DefaultModalGraphMouse<Argument, DTreeEdge>();
		
	}


	private void initVisualizerViewer(){
		this.vv = new VisualizationViewer<Argument, DTreeEdge>(this.layout);
		
		
		this.vv.setGraphMouse(this.mouse);
		
		
		JMenu menu = this.mouse.getModeMenu();
		menu.setText("MOUSE MODE");
		menu.setPreferredSize(new Dimension(200, 20));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(menu);
		
		this.add(menuBar, BorderLayout.NORTH);
		
		this.mouse.setMode(Mode.PICKING);
		
		this.vv.getRenderContext().setVertexFillPaintTransformer(new Transformer<Argument, Paint>() {

			@Override
			public Paint transform(Argument v) {
				return v.isAccepted() ? acceptedArgumentColor : rejectedArgumentColor;
			}
		});
		
		
		this.vv.getRenderContext().setEdgeLabelTransformer(new Transformer<DTreeEdge, String>(){
				
			@Override
			public String transform(DTreeEdge edge){
				return "";
			}
			
		});
	
	
		this.vv.getRenderContext().setVertexLabelTransformer(new Transformer<Argument, String>(){

			@Override
			public String transform(Argument vertex) {
				
				String label = "";
				
				if(vertex != null){
					label += "A"+vertex.getId()+":<"+vertex.getClaim()+";"+vertex.getRule().toString()+">";
				}
				
				return label;
			}
			
		});
		
		this.vv.getRenderContext().setVertexShapeTransformer(new ArgumentShapeTransformer());
		
		this.vv.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent arg0) {
				super.componentResized(arg0);
				Utils.fitGraph(vv, layout);
			}});
	}
	
	public void loadGraph(){
		this.graph.load();
		
		this.layout = new  ISOMLayout<Argument, DTreeEdge>(this.graph.getGraph());
		
		if(this.vv != null){
			this.remove(this.vv);
		}
			
		initVisualizerViewer();
		
		this.add(this.vv, BorderLayout.CENTER);
	}
	
	public void clearGraph(){
		this.graph.clearGraph();
		this.vv.repaint();
	}
	
	
	private class ArgumentShapeTransformer extends EllipseVertexShapeTransformer<Argument>{
		
		ArgumentShapeTransformer() {
            setSizeTransformer(new Transformer<Argument, Integer>() {
            	
            	public Integer transform(Argument v){
            		return 30;
            	}
			});
        }
		
		public Shape transform(Argument v) {
            return factory.getRegularPolygon(v, 3);
        }
	}
}

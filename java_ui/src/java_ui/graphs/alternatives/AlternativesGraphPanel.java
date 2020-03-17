package java_ui.graphs.alternatives;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.ISOMLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;

public class AlternativesGraphPanel extends JPanel {
	
	private AlternativesGraph graph;
	private ISOMLayout<AlternativesGraphVertex, AlternativesGraphEdge> layout;
	private VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge> vv;
	private DefaultModalGraphMouse<AlternativesGraphVertex, AlternativesGraphEdge> mouse;
	
	public AlternativesGraphPanel(){
		this.graph = new AlternativesGraph();
		setLayout(new BorderLayout());
		
		this.mouse = new DefaultModalGraphMouse<AlternativesGraphVertex, AlternativesGraphEdge>();
		
	}
	
	
	private void initVisualizerViewer(){
		this.vv = new VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge>(this.layout);
		
		
		this.vv.setGraphMouse(this.mouse);	
		
		this.mouse.add(new AlternativesGraphPopupMenuMousePlugin(this.graph, this.vv));
		
		JMenu menu = this.mouse.getModeMenu();
		menu.setText("MOUSE MODE");
		menu.setPreferredSize(new Dimension(200, 20));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(menu);
		
		this.add(menuBar, BorderLayout.NORTH);
		
		this.mouse.setMode(Mode.PICKING);
		
		this.vv.getRenderContext().setVertexFillPaintTransformer(new Transformer<AlternativesGraphVertex, Paint>() {

			@Override
			public Paint transform(AlternativesGraphVertex v) {
				if(vv.getPickedVertexState().isPicked(v)){
					return Color.CYAN;
				}
				else{
					if(v.isSelected()){
						return Color.WHITE;
					}
					else{
						return Color.DARK_GRAY;
					}
				}
				
			}
		});
		
		
		this.vv.getRenderContext().setEdgeLabelTransformer(new Transformer<AlternativesGraphEdge, String>(){
				
			@Override
			public String transform(AlternativesGraphEdge edge){
				return edge.toString();
			}
			
		});
	
	
		this.vv.getRenderContext().setVertexLabelTransformer(new Transformer<AlternativesGraphVertex, String>(){

			@Override
			public String transform(AlternativesGraphVertex vertex) {
				return vertex.getId();
			}
			
		});
	}
	
	public void loadGraph(){
		this.graph.load();
		
		this.layout = new  ISOMLayout<AlternativesGraphVertex, AlternativesGraphEdge>(this.graph.getGraph());
		
		if(this.vv != null){
			this.remove(this.vv);
		}
			
		initVisualizerViewer();
		
		this.add(this.vv, BorderLayout.CENTER);
	}
	
	public void clearGraph(){
		if(this.graph != null) {
			this.graph.clearGraph();
			if(this.layout != null) {
				this.layout.setGraph(this.graph.getGraph());
				if(this.vv != null) {
					vv.setGraphLayout(layout);
				}
			}
		}
		
	}
}

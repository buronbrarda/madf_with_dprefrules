package java_ui.graphs.alternatives;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Paint;

import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.KKLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;

public class AlternativesGraphPanel extends JPanel {
	
	private AlternativesGraph graph;
	private KKLayout<AlternativesGraphVertex, AlternativesGraphEdge> layout;
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
		
		this.mouse.setMode(Mode.PICKING);
		
		
	
		this.vv.getRenderContext().setVertexFillPaintTransformer(new Transformer<AlternativesGraphVertex, Paint>() {

			@Override
			public Paint transform(AlternativesGraphVertex v) {
				if(vv.getPickedVertexState().isPicked(v)){
					return Color.YELLOW;
				}
				else{
					if(v.isSelected()){
						return Color.GREEN;
					}
					else{
						return Color.RED;
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
		
		this.layout = new  KKLayout<AlternativesGraphVertex, AlternativesGraphEdge>(this.graph.getGraph());
		this.layout.setAdjustForGravity(true);
		
		if(this.vv != null){
			this.remove(this.vv);
		}
			
		initVisualizerViewer();
		
		this.add(this.vv, BorderLayout.CENTER);
	}
	
	public void clearGraph(){
		this.graph.clearGraph();
		this.layout.setGraph(this.graph.getGraph());
		vv.setGraphLayout(layout);
	}
}

package java_ui.graphs.alternatives;

import java.awt.BorderLayout;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.DAGLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;

public class AlternativesGraphPanel extends JPanel {
	
	private AlternativesGraph graph;
	private DAGLayout<AlternativesGraphVertex, AlternativesGraphEdge> layout;
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
		
		this.layout = new  DAGLayout<AlternativesGraphVertex, AlternativesGraphEdge>(this.graph.getGraph());
		
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

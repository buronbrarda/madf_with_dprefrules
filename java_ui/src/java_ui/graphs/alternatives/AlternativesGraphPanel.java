package java_ui.graphs.alternatives;

import java.awt.BorderLayout;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;

public class AlternativesGraphPanel extends JPanel {
	
	private AlternativesGraph graph;
	private FRLayout<AlternativesGraphVertex, AlternativesGraphEdge> layout;
	private VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge> vv;
	
	private JPanel graphContainer;
	
	public AlternativesGraphPanel(){
		this.graph = new AlternativesGraph();
		
		graphContainer = new JPanel();
		graphContainer.setLayout(new BorderLayout());	
		
		this.setLocation(0, 0);
		this.setLayout(new BorderLayout());
		this.add(graphContainer, BorderLayout.CENTER);
		
		
		this.layout = new  FRLayout<AlternativesGraphVertex, AlternativesGraphEdge>(this.graph.getGraph());
		this.layout.setAttractionMultiplier(0.1);
		this.layout.setRepulsionMultiplier(5);
		
		initVisualizerViewer();
		
		graphContainer.add(vv, BorderLayout.CENTER);
		this.vv.setLocation(0, 0);
	}
	
	
	private void initVisualizerViewer(){
		this.vv = new VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge>(this.layout);
		
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
		
		this.layout.setSize(this.graphContainer.getSize());
		
		this.vv.setGraphLayout(layout);
	}
	
	public void clearGraph(){
		this.graph.clearGraph();
		this.layout.setGraph(this.graph.getGraph());
		vv.setGraphLayout(layout);
	}
}

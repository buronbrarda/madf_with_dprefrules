package java_ui.graphs.alternatives;

import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.AbstractPopupGraphMousePlugin;
import java_ui.graphs.alternatives.lattice.JungLatticeLayout;

public class AlternativesGraphPopupMenuMousePlugin extends AbstractPopupGraphMousePlugin{
	
	private AlternativesGraphCompoundVertexPopupMenu vertexPopup;
	private AlternativesGraphEdgePopupMenu edgePopup;
	private VisualizationViewer<AlternativesGraphVertex,AlternativesGraphEdge> vv;
	
	public AlternativesGraphPopupMenuMousePlugin(AlternativesGraph graph, VisualizationViewer<AlternativesGraphVertex,AlternativesGraphEdge> vv, JungLatticeLayout<AlternativesGraphVertex,AlternativesGraphEdge> layout){
		this(MouseEvent.BUTTON3, graph, vv, layout);
	}
	
	public AlternativesGraphPopupMenuMousePlugin(int modifiers,AlternativesGraph graph, VisualizationViewer<AlternativesGraphVertex,AlternativesGraphEdge> vv, JungLatticeLayout<AlternativesGraphVertex,AlternativesGraphEdge> layout){
		super(modifiers);
		this.vv = vv;
		vertexPopup = new AlternativesGraphCompoundVertexPopupMenu(graph, vv, layout);
		edgePopup = new AlternativesGraphEdgePopupMenu(graph);
	}
	
	
	@Override
	protected void handlePopup(MouseEvent e) {
		
        Point2D p = e.getPoint();
        
        GraphElementAccessor<AlternativesGraphVertex,AlternativesGraphEdge> pickSupport = vv.getPickSupport();
        
        if(pickSupport != null) {
        	
            AlternativesGraphEdge edge = pickSupport.getEdge(vv.getGraphLayout(), p.getX(), p.getY());
        	if(edge != null){
        		if(edge instanceof AlternativesGraphStrongEdge){
            		edgePopup.setEdge(edge);
            		edgePopup.show(vv,(int)p.getX(),(int)p.getY());
            	}
        		
        	}
        	else{
        		AlternativesGraphVertex v = pickSupport.getVertex(vv.getGraphLayout(), p.getX(), p.getY());
	        	if(v != null) {
	            	if(v instanceof AlternativesGraphVertex){
	            		vertexPopup.setPicked(vv.getPickedVertexState().getPicked());
	            		vertexPopup.setVertex(v);
	            		vertexPopup.show(vv,(int)p.getX(),(int)p.getY());
	            	}
	            }
        	}
		
        }
        
	}

}

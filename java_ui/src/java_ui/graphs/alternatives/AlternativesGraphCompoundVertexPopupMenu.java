package java_ui.graphs.alternatives;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import edu.uci.ics.jung.visualization.VisualizationViewer;

public class AlternativesGraphCompoundVertexPopupMenu extends JPopupMenu{
	
	private AlternativesGraphCompoundVertex v;
	
	public AlternativesGraphCompoundVertexPopupMenu(AlternativesGraph graph, VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge> vv) {
		
		super();
		
		JMenuItem expandItem = new JMenuItem("Expand");
		expandItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				graph.expand(v);
				vv.repaint();
			}
		});
		
		this.add(expandItem);
		
	}
	
	public void setVertex(AlternativesGraphCompoundVertex v){
		this.v = v;
	}
	
	
}

package java_ui.graphs.alternatives;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Set;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import edu.uci.ics.jung.visualization.VisualizationViewer;

public class AlternativesGraphCompoundVertexPopupMenu extends JPopupMenu{
	
	private AlternativesGraphVertex v;
	private JMenuItem expandItem;
	private JMenuItem addEdgesItem;
	private Set<AlternativesGraphVertex> picked;
	
	public AlternativesGraphCompoundVertexPopupMenu(final AlternativesGraph graph, final VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge> vv) {
		
		super();
		
		expandItem = new JMenuItem("Expand");
		expandItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				graph.expand((AlternativesGraphCompoundVertex)v);
				vv.repaint();
			}
		});
		
		addEdgesItem = new JMenuItem("Add extra-edges");
		addEdgesItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				graph.addExtraEdgesBetween(picked);
				vv.repaint();
			}
		});
		
		this.add(expandItem);
		this.add(addEdgesItem);
		
	}
	
	public void setVertex(AlternativesGraphVertex v){
		this.v = v;	
		expandItem.setEnabled(v instanceof AlternativesGraphCompoundVertex);		
	}

	public void setPicked(Set<AlternativesGraphVertex> picked) {
		this.picked = picked;	
	}
	
	
}

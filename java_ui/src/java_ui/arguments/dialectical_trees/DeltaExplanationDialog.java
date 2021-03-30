package java_ui.arguments.dialectical_trees;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.ArrayList;

import javax.swing.JDialog;
import javax.swing.JPanel;

import java_ui.arguments.Argument;

public class DeltaExplanationDialog extends JDialog {
	
	private Dimension defaultDimension = new Dimension(500,500);
	
	private DeltaExplanationPanel graphPanel;
	
	
	public DeltaExplanationDialog(ArrayList<Argument> arguments){
		
		this.setTitle("Delta-explanation - (Prototype Version)");
		this.setModalityType(ModalityType.MODELESS);
		
		this.setSize(defaultDimension);
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		JPanel graphContainer = new JPanel();
		
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(graphContainer, BorderLayout.CENTER);
		graphContainer.setLayout(new BorderLayout(0, 0));
		
		graphPanel = new DeltaExplanationPanel();
		graphContainer.add(graphPanel);
		
		graphPanel.loadGraph(arguments);
	}
	
	public DeltaExplanationPanel getGraphPanel(){
		return this.graphPanel;
	}
}

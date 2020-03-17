package java_ui.arguments.dialectical_trees;

import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JDialog;
import javax.swing.JPanel;

public class ArgumentsGraphDialog extends JDialog {
	
	private Dimension defaultDimension = new Dimension(500,500);
	
	private ArgumentsGraphPanel graphPanel;
	
	
	public ArgumentsGraphDialog(){
		
		this.setTitle("Arguments Graph - (Prototype Version)");
		this.setModalityType(ModalityType.MODELESS);
		
		this.setSize(defaultDimension);
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		JPanel graphContainer = new JPanel();
		
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(graphContainer, BorderLayout.CENTER);
		graphContainer.setLayout(new BorderLayout(0, 0));
		
		graphPanel = new ArgumentsGraphPanel();
		graphContainer.add(graphPanel);
		
		graphPanel.loadGraph();
	}
}
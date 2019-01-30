package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import javax.swing.JSeparator;
import javax.swing.JButton;

public class StepsView extends JPanel{
	
	
	public StepsView(){
		super();
		setLayout(new BorderLayout(0, 0));
		
		JPanel stepsPanel = new JPanel();
		add(stepsPanel);
		stepsPanel.setLayout(new GridLayout(3, 1, 0, 0));
		
		JPanel dataLoadingPanel = new JPanel();
		stepsPanel.add(dataLoadingPanel);
		dataLoadingPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel knowledgeLoadingPanel = new KnowledgeLoadingPanel();
		dataLoadingPanel.add(knowledgeLoadingPanel);
		
		JSeparator separator = new JSeparator();
		dataLoadingPanel.add(separator, BorderLayout.SOUTH);
		
		JPanel cprefRulesPanel = new JPanel();
		stepsPanel.add(cprefRulesPanel);
		cprefRulesPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel cprefRulesLoadingPanel = new CPrefRulesLoadingPanel();
		cprefRulesPanel.add(cprefRulesLoadingPanel);
		
		JSeparator separator_1 = new JSeparator();
		cprefRulesPanel.add(separator_1, BorderLayout.SOUTH);
		
		JPanel lastPanel = new JPanel();
		stepsPanel.add(lastPanel);
		
		JButton btnNewButton = new JButton("Run");
		lastPanel.add(btnNewButton);
	}
	
	
	
}

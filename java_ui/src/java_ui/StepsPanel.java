package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import javax.swing.JSeparator;
import javax.swing.JButton;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;

public class StepsPanel extends JPanel{
	
	
	public StepsPanel(){
		super();
		setLayout(new BorderLayout(0, 0));
		
		JPanel stepsPanel = new JPanel();
		add(stepsPanel);
		GridBagLayout gbl_stepsPanel = new GridBagLayout();
		gbl_stepsPanel.columnWidths = new int[]{225, 0};
		gbl_stepsPanel.rowHeights = new int[]{80, 80, 0, 0, 0};
		gbl_stepsPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_stepsPanel.rowWeights = new double[]{1.0, 1.0, 1.0, 0.0, Double.MIN_VALUE};
		stepsPanel.setLayout(gbl_stepsPanel);
		
		JPanel criteriaLoadingPanel = new JPanel();
		GridBagConstraints gbc_criteriaLoadingPanel = new GridBagConstraints();
		gbc_criteriaLoadingPanel.fill = GridBagConstraints.BOTH;
		gbc_criteriaLoadingPanel.insets = new Insets(0, 0, 5, 0);
		gbc_criteriaLoadingPanel.gridx = 0;
		gbc_criteriaLoadingPanel.gridy = 0;
		stepsPanel.add(criteriaLoadingPanel, gbc_criteriaLoadingPanel);
		criteriaLoadingPanel.setLayout(new BorderLayout(0, 0));
		
		CriteriaLoadingPanel loadingPanel = new CriteriaLoadingPanel();
		criteriaLoadingPanel.add(loadingPanel, BorderLayout.CENTER);
		
		JSeparator separator_2 = new JSeparator();
		criteriaLoadingPanel.add(separator_2, BorderLayout.SOUTH);
		
		JPanel dataLoadingPanel = new JPanel();
		GridBagConstraints gbc_dataLoadingPanel = new GridBagConstraints();
		gbc_dataLoadingPanel.fill = GridBagConstraints.BOTH;
		gbc_dataLoadingPanel.insets = new Insets(0, 0, 5, 0);
		gbc_dataLoadingPanel.gridx = 0;
		gbc_dataLoadingPanel.gridy = 1;
		stepsPanel.add(dataLoadingPanel, gbc_dataLoadingPanel);
		dataLoadingPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel knowledgeLoadingPanel = new KnowledgeLoadingPanel();
		dataLoadingPanel.add(knowledgeLoadingPanel);
		
		JSeparator separator = new JSeparator();
		dataLoadingPanel.add(separator, BorderLayout.SOUTH);
		
		JPanel cprefRulesPanel = new JPanel();
		GridBagConstraints gbc_cprefRulesPanel = new GridBagConstraints();
		gbc_cprefRulesPanel.fill = GridBagConstraints.BOTH;
		gbc_cprefRulesPanel.insets = new Insets(0, 0, 5, 0);
		gbc_cprefRulesPanel.gridx = 0;
		gbc_cprefRulesPanel.gridy = 2;
		stepsPanel.add(cprefRulesPanel, gbc_cprefRulesPanel);
		cprefRulesPanel.setLayout(new BorderLayout(0, 0));
		
		JPanel cprefRulesLoadingPanel = new CPrefRulesLoadingPanel();
		cprefRulesPanel.add(cprefRulesLoadingPanel);
		
		JSeparator separator_1 = new JSeparator();
		cprefRulesPanel.add(separator_1, BorderLayout.SOUTH);
		
		JPanel lastPanel = new JPanel();
		GridBagConstraints gbc_lastPanel = new GridBagConstraints();
		gbc_lastPanel.fill = GridBagConstraints.BOTH;
		gbc_lastPanel.gridx = 0;
		gbc_lastPanel.gridy = 3;
		stepsPanel.add(lastPanel, gbc_lastPanel);
		
		JButton btnNewButton = new JButton("Run");
		lastPanel.add(btnNewButton);
	}
	
	
	
}

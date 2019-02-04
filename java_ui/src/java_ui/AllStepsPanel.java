package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JSeparator;

import java_ui.cpref_rules_edition.CPrefRulesTableEditorPanel;
import java_ui.criteria_edition.CriteriaTableEditorPanel;
import java_ui.knowledge_edition.KnowledgeTableEditorPanel;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.io.IOException;

public class AllStepsPanel extends JPanel{
	
	
	public AllStepsPanel() throws IOException{
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
		
		StepPanel step_1, step_2, step_3, run_step;
		
		step_1 = new DefineStepPanel(
				"1. Define the set of criteria and their assessment values.",
				new CriteriaTableEditorPanel(),
				new CriteriaPrologLoader()
		);
		
		step_2 = new DefineStepPanel(
				"2. Define the assessments knowledge base.",
				new KnowledgeTableEditorPanel(),
				new KnowledgePrologLoader()
		);
		
		step_3 = new DefineStepPanel(
				"3. Define the set of CPref-Rules.",
				new CPrefRulesTableEditorPanel(),
				new CPrefRulesPrologLoader()
		);
		
		run_step = new RunStepPanel();
		
		
		step_1.setFollowingStep(step_2);
		step_2.setFollowingStep(step_3);
		step_3.setFollowingStep(run_step);
		
		step_2.disableStep();
		
		StepPanel [] stepsPanels = {step_1, step_2, step_3};
		
		int i = 0;
		for(i = 0; i < stepsPanels.length; i++) {
			JPanel container = new JPanel();
			GridBagConstraints gbc_container = new GridBagConstraints();
			gbc_container.fill = GridBagConstraints.BOTH;
			gbc_container.insets = new Insets(0, 0, 5, 0);
			gbc_container.gridx = 0;
			gbc_container.gridy = i;
			stepsPanel.add(container, gbc_container);
			container.setLayout(new BorderLayout(0, 0));
			
			container.add(stepsPanels[i], BorderLayout.CENTER);
			container.add(new JSeparator(), BorderLayout.SOUTH);
			
		}
		
		GridBagConstraints gbc_lastPanel = new GridBagConstraints();
		gbc_lastPanel.fill = GridBagConstraints.BOTH;
		gbc_lastPanel.gridx = 0;
		gbc_lastPanel.gridy = i;
		stepsPanel.add(run_step, gbc_lastPanel);
	
	}
	
}

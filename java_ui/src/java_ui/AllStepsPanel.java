package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JSeparator;

import java_ui.prolog_loader.CPrefRulesPrologLoader;
import java_ui.prolog_loader.CriteriaPrologLoader;
import java_ui.prolog_loader.EvidencePrologLoader;
import java_ui.prolog_loader.RuleImportancePrologLoader;
import java_ui.steps.DefineCprefRulesStepPanel;
import java_ui.steps.DefineEvidenceStepPanel;
import java_ui.steps.DefineRuleImportanceStepPanel;
import java_ui.steps.DefineStepPanel;
import java_ui.steps.ResultsPanel;
import java_ui.steps.RunStepPanel;
import java_ui.table_editor.panel.CPrefRulesTableEditorPanel;
import java_ui.table_editor.panel.CriteriaTableEditorPanel;
import java_ui.table_editor.panel.EvidenceTableEditorPanel;
import java_ui.table_editor.panel.RuleImportanceTableEditorPanel;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.io.IOException;

public class AllStepsPanel extends JPanel{
	
	private DefineStepPanel step1;
	private DefineEvidenceStepPanel step2;
	private DefineCprefRulesStepPanel step3;
	private DefineRuleImportanceStepPanel step4;
	private RunStepPanel runStep;
	private JPanel panel;
	
	
	
	public AllStepsPanel(ResultsPanel resultsPanel) throws IOException{
		super();
		setLayout(new BorderLayout(0, 0));
		
		JPanel stepsPanel = new JPanel();
		add(stepsPanel);
		GridBagLayout gbl_stepsPanel = new GridBagLayout();
		gbl_stepsPanel.columnWidths = new int[]{313, 0};
		gbl_stepsPanel.rowHeights = new int[]{57, 64, 87, 0, 36};
		gbl_stepsPanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_stepsPanel.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 1.0};
		stepsPanel.setLayout(gbl_stepsPanel);
		
		
		
		step1 = new DefineStepPanel(
				"1. Define the set of criteria and their range of values.",
				"Criteria",
				new CriteriaTableEditorPanel(),
				new CriteriaPrologLoader()
		);
		
		
		step2 = new DefineEvidenceStepPanel(
				"2. Define the set of evidence.",
				"Evidence",
				new EvidenceTableEditorPanel(),
				new EvidencePrologLoader()
		);
		
		
		step3 = new DefineCprefRulesStepPanel(
				"3. Define the set of CPref-Rules.",
				"CPref-Rules",
				new CPrefRulesTableEditorPanel(),
				new CPrefRulesPrologLoader()
		);
		
		step4 = new DefineRuleImportanceStepPanel(
				"4. Define the agent's priority order, and the importance"+"\n"+"each agent assigns to the CPref-rules.",
				"Importance Orders",
				new RuleImportanceTableEditorPanel(),
				new RuleImportancePrologLoader()
		);
		
		
		runStep = new RunStepPanel(step1, resultsPanel);
		
		
		
		step1.setFollowingStep(step2);
		step2.setFollowingStep(step3);
		step3.setFollowingStep(step4);
		step4.setFollowingStep(runStep);
		
		step2.disableStep();
		
		
		panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.fill = GridBagConstraints.BOTH;
		gbc_panel.insets = new Insets(0, 0, 5, 0);
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 0;
		stepsPanel.add(panel, gbc_panel);
		GridBagLayout gbl_panel = new GridBagLayout();
		gbl_panel.columnWidths = new int[]{50, 0};
		gbl_panel.rowHeights = new int[]{57, 0};
		gbl_panel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);
		
		
		ExamplesLoadPanel examplesLoadPanel = new ExamplesLoadPanel(step1, step2, step3, step4, runStep);
		GridBagConstraints gbc_examplesLoadPanel = new GridBagConstraints();
		gbc_examplesLoadPanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_examplesLoadPanel.anchor = GridBagConstraints.NORTH;
		gbc_examplesLoadPanel.gridx = 0;
		gbc_examplesLoadPanel.gridy = 0;
		panel.add(examplesLoadPanel, gbc_examplesLoadPanel);
		
		
		GridBagConstraints gbc_container;
		GridBagConstraints gbc_container_1;
		GridBagConstraints gbc_container_2;
		GridBagConstraints gbc_container_3;
		
		//------------------------------ Step 1 -------------------------------------

		
		JPanel container = new JPanel();
		gbc_container = new GridBagConstraints();
		gbc_container.fill = GridBagConstraints.HORIZONTAL;
		gbc_container.anchor = GridBagConstraints.NORTH;
		gbc_container.insets = new Insets(0, 0, 5, 0);
		gbc_container.gridx = 0;
		gbc_container.gridy = 1;
		stepsPanel.add(container, gbc_container);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step1, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		//------------------------------ Step 2 -------------------------------------

		
		container = new JPanel();
		gbc_container_1 = new GridBagConstraints();
		gbc_container_1.fill = GridBagConstraints.HORIZONTAL;
		gbc_container_1.anchor = GridBagConstraints.NORTH;
		gbc_container_1.insets = new Insets(0, 0, 5, 0);
		gbc_container_1.gridx = 0;
		gbc_container_1.gridy = 2;
		stepsPanel.add(container, gbc_container_1);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step2, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		//------------------------------ Step 3 -------------------------------------
		
		container = new JPanel();
		gbc_container_2 = new GridBagConstraints();
		gbc_container_2.fill = GridBagConstraints.HORIZONTAL;
		gbc_container_2.anchor = GridBagConstraints.NORTH;
		gbc_container_2.insets = new Insets(0, 0, 5, 0);
		gbc_container_2.gridx = 0;
		gbc_container_2.gridy = 3;
		stepsPanel.add(container, gbc_container_2);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step3, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		//------------------------------ Step 4 -------------------------------------
		
		container = new JPanel();
		gbc_container_3 = new GridBagConstraints();
		gbc_container_3.fill = GridBagConstraints.HORIZONTAL;
		gbc_container_3.anchor = GridBagConstraints.NORTH;
		gbc_container_3.insets = new Insets(0, 0, 5, 0);
		gbc_container_3.gridx = 0;
		gbc_container_3.gridy = 4;
		stepsPanel.add(container, gbc_container_3);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step4, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		//------------------------------ Run Step -------------------------------------

		
		GridBagConstraints gbc_lastPanel = new GridBagConstraints();
		gbc_lastPanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_lastPanel.anchor = GridBagConstraints.NORTH;
		gbc_lastPanel.gridx = 0;
		gbc_lastPanel.gridy = 5;
		stepsPanel.add(runStep, gbc_lastPanel);
	
	}
	
	
	
	
	public void cleanSteps(){
		if(this.step1 != null){
			this.step1.cleanStep();
		}
	}
	
}

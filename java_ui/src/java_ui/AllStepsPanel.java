package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JSeparator;

import java_ui.ExamplesLoadPanel;
import java_ui.prolog_loader.CPrefRulesPrologLoader;
import java_ui.prolog_loader.CriteriaPrologLoader;
import java_ui.prolog_loader.EvidencePrologLoader;
import java_ui.steps.DefineCprefRulesStepPanel;
import java_ui.steps.DefineEvidenceStepPanel;
import java_ui.steps.DefineStepPanel;
import java_ui.steps.ResultsPanel;
import java_ui.steps.RunStepPanel;
import java_ui.table_editor.panel.CPrefRulesTableEditorPanel;
import java_ui.table_editor.panel.CriteriaTableEditorPanel;
import java_ui.table_editor.panel.EvidenceTableEditorPanel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.io.IOException;

public class AllStepsPanel extends JPanel{
	
	private DefineStepPanel step_1;
	private DefineEvidenceStepPanel step_2;
	private DefineCprefRulesStepPanel step_3;
	private RunStepPanel run_step;
	private GridBagConstraints gbc_container_1;
	private GridBagConstraints gbc_container_2;
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
		
		
		
		step_1 = new DefineStepPanel(
				"1. Define the set of criteria and their range of values.",
				"Criteria",
				new CriteriaTableEditorPanel(),
				new CriteriaPrologLoader()
		);
		
		
		step_2 = new DefineEvidenceStepPanel(
				"2. Define the set of evidence.",
				"Evidence",
				new EvidenceTableEditorPanel(),
				new EvidencePrologLoader()
		);
		
		
		step_3 = new DefineCprefRulesStepPanel(
				"3. Define the set of CPref-Rules.",
				"CPref-Rules",
				new CPrefRulesTableEditorPanel(),
				new CPrefRulesPrologLoader()
		);
		
		
		
		run_step = new RunStepPanel(step_1, resultsPanel);
		
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
		
		
		ExamplesLoadPanel examplesLoadPanel = new ExamplesLoadPanel(step_1, step_2, step_3, run_step);
		GridBagConstraints gbc_examplesLoadPanel = new GridBagConstraints();
		gbc_examplesLoadPanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_examplesLoadPanel.anchor = GridBagConstraints.NORTH;
		gbc_examplesLoadPanel.gridx = 0;
		gbc_examplesLoadPanel.gridy = 0;
		panel.add(examplesLoadPanel, gbc_examplesLoadPanel);
		
		step_1.setFollowingStep(step_2);
		step_2.setFollowingStep(step_3);
		step_3.setFollowingStep(run_step);
		
		step_2.disableStep();
		
		
		GridBagConstraints gbc_container;
		
		JPanel container = new JPanel();
		gbc_container = new GridBagConstraints();
		gbc_container.anchor = GridBagConstraints.NORTHWEST;
		gbc_container.insets = new Insets(0, 0, 5, 0);
		gbc_container.gridx = 0;
		gbc_container.gridy = 1;
		stepsPanel.add(container, gbc_container);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step_1, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		container = new JPanel();
		gbc_container_1 = new GridBagConstraints();
		gbc_container_1.anchor = GridBagConstraints.NORTHWEST;
		gbc_container_1.insets = new Insets(0, 0, 5, 0);
		gbc_container_1.gridx = 0;
		gbc_container_1.gridy = 2;
		stepsPanel.add(container, gbc_container_1);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step_2, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		container = new JPanel();
		gbc_container_2 = new GridBagConstraints();
		gbc_container_2.anchor = GridBagConstraints.NORTHWEST;
		gbc_container_2.insets = new Insets(0, 0, 5, 0);
		gbc_container_2.gridx = 0;
		gbc_container_2.gridy = 3;
		stepsPanel.add(container, gbc_container_2);
		container.setLayout(new BorderLayout(0, 0));
		
		container.add(step_3, BorderLayout.CENTER);
		container.add(new JSeparator(), BorderLayout.SOUTH);
		
		
		GridBagConstraints gbc_lastPanel = new GridBagConstraints();
		gbc_lastPanel.anchor = GridBagConstraints.NORTHWEST;
		gbc_lastPanel.gridx = 0;
		gbc_lastPanel.gridy = 4;
		stepsPanel.add(run_step, gbc_lastPanel);
	
	}
	
	
	
	
	public void cleanSteps(){
		if(this.step_1 != null){
			this.step_1.cleanStep();
		}
	}
	
}

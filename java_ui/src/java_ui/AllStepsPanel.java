package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JSeparator;
import javax.swing.table.TableModel;

import java_ui.prolog_loader.CPrefRulesPrologLoader;
import java_ui.prolog_loader.CriteriaPrologLoader;
import java_ui.prolog_loader.EvidencePrologLoader;
import java_ui.prolog_loader.FeaturesPrologLoader;
import java_ui.prolog_loader.ProfileRulesPrologLoader;
import java_ui.steps.DefineStepPanel;
import java_ui.steps.ResultsPanel;
import java_ui.steps.RunStepPanel;
import java_ui.steps.StepPanel;
import java_ui.table_editor.model_builder.RulesTableModelBuilder;
import java_ui.table_editor.model_builder.CriteriaTableModelBuilder;
import java_ui.table_editor.model_builder.EvidenceTableModelBuilder;
import java_ui.table_editor.panel.CPrefRulesTableEditorPanel;
import java_ui.table_editor.panel.CriteriaTableEditorPanel;
import java_ui.table_editor.panel.EvidenceTableEditorPanel;
import java_ui.table_editor.panel.FeaturesTableEditorPanel;
import java_ui.table_editor.panel.ProfileRulesTableEditorPanel;
import java_ui.table_editor.table_reader.CSVTableReader;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.io.File;
import java.io.IOException;

public class AllStepsPanel extends JPanel{
	
	DefineStepPanel step_1, step_2, step_3, step_4, step_5;
	RunStepPanel run_step;
	
	public AllStepsPanel(ResultsPanel resultsPanel) throws IOException{
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
		
		
		
		JPanel examplesLoadPanel = new ExamplesLoadPanel(this);
		GridBagConstraints gbc_examplesLoadPanel = new GridBagConstraints();
		gbc_examplesLoadPanel.fill = GridBagConstraints.BOTH;
		gbc_examplesLoadPanel.insets = new Insets(0, 0, 5, 0);
		gbc_examplesLoadPanel.gridx = 0;
		gbc_examplesLoadPanel.gridy = 0;
		stepsPanel.add(examplesLoadPanel, gbc_examplesLoadPanel);
		
		
		step_1 = new DefineStepPanel(
				"1. Define the set of features and their domain.",
				new FeaturesTableEditorPanel(),
				new FeaturesPrologLoader()
		);
		
		step_2 = new DefineStepPanel(
				"2. Define the set of evidence.",
				new EvidenceTableEditorPanel(),
				new EvidencePrologLoader()
		);
		
		step_3 = new DefineStepPanel(
				"3. Define the set of criteria and their assessment values.",
				new CriteriaTableEditorPanel(),
				new CriteriaPrologLoader()
		);
		
		step_4 = new DefineStepPanel(
				"4. Define the set of Profile-Rules.",
				new ProfileRulesTableEditorPanel(),
				new ProfileRulesPrologLoader()
		);
		
		step_5 = new DefineStepPanel(
				"5. Define the set of CPref-Rules.",
				new CPrefRulesTableEditorPanel(),
				new CPrefRulesPrologLoader()
		);
		
		run_step = new RunStepPanel(step_1, resultsPanel);
		
		
		step_1.setFollowingStep(step_2);
		step_2.setFollowingStep(step_3);
		step_3.setFollowingStep(step_4);
		step_4.setFollowingStep(step_5);
		step_5.setFollowingStep(run_step);
		
		step_2.disableStep();
		
		StepPanel [] stepsPanels = {step_1, step_2, step_3, step_4, step_5};
		
		int i;
		for(i = 0; i < stepsPanels.length; i++) {
			JPanel container = new JPanel();
			GridBagConstraints gbc_container = new GridBagConstraints();
			gbc_container.fill = GridBagConstraints.BOTH;
			gbc_container.insets = new Insets(0, 0, 5, 0);
			gbc_container.gridx = 0;
			gbc_container.gridy = i+1;
			stepsPanel.add(container, gbc_container);
			container.setLayout(new BorderLayout(0, 0));
			
			container.add(stepsPanels[i], BorderLayout.CENTER);
			container.add(new JSeparator(), BorderLayout.SOUTH);
			
		}
		
		GridBagConstraints gbc_lastPanel = new GridBagConstraints();
		gbc_lastPanel.fill = GridBagConstraints.BOTH;
		gbc_lastPanel.gridx = 0;
		gbc_lastPanel.gridy = i+1;
		stepsPanel.add(run_step, gbc_lastPanel);
	
	}
	
	
	public void loadExample(int n, String subject) throws IOException{
		
		String features_example_path = "features_example.csv";
		String criteria_example_path = "criteria_example.csv";
		String evidence_example_path = "evidence_example_"+n+".csv";
		String profile_rules_example_path = "profile_rules_example.csv";
		String cpref_rules_example_path = "cpref_rules_example ("+subject+").csv";
		
		File features_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+features_example_path);
		File criteria_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+criteria_example_path);
		File evidence_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+evidence_example_path);
		File profile_rules_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+profile_rules_example_path);
		File cpref_rules_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+cpref_rules_example_path);
		
		TableModel featuresModel = new CriteriaTableModelBuilder(new CSVTableReader(features_file)).getTableModel();
		TableModel criteriaModel = new CriteriaTableModelBuilder(new CSVTableReader(criteria_file)).getTableModel();
		TableModel evidenceModel = new EvidenceTableModelBuilder(new CSVTableReader(evidence_file)).getTableModel();
		TableModel cprefRulesModel = new RulesTableModelBuilder(new CSVTableReader(cpref_rules_file)).getTableModel();
		TableModel profileRulesModel = new RulesTableModelBuilder(new CSVTableReader(profile_rules_file)).getTableModel();
		
		step_1.setTableModel(featuresModel);
		step_2.setTableModel(evidenceModel);
		step_3.setTableModel(criteriaModel);
		step_4.setTableModel(profileRulesModel);
		step_5.setTableModel(cprefRulesModel);
		
		
		run_step.enableStep();
		
	}
	
	public void cleanSteps(){
		if(this.step_1 != null){
			this.step_1.cleanStep();
		}
	}
	
}

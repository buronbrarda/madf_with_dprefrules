package java_ui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JSeparator;
import javax.swing.table.TableModel;

import java_ui.PrologLoader.CPrefRulesPrologLoader;
import java_ui.PrologLoader.CriteriaPrologLoader;
import java_ui.PrologLoader.KnowledgePrologLoader;
import java_ui.cpref_rules_edition.CPrefRulesTableEditorPanel;
import java_ui.cpref_rules_edition.CPrefRulesTableModelBuilder;
import java_ui.criteria_edition.CriteriaTableEditorPanel;
import java_ui.criteria_edition.CriteriaTableModelBuilder;
import java_ui.knowledge_edition.EvidenceTableModelBuilder;
import java_ui.knowledge_edition.KnowledgeTableEditorPanel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.io.File;
import java.io.IOException;

public class AllStepsPanel extends JPanel{
	
	DefineStepPanel step_1, step_2, step_3;
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
		
		run_step = new RunStepPanel(resultsPanel);
		
		
		step_1.setFollowingStep(step_2);
		step_2.setFollowingStep(step_3);
		step_3.setFollowingStep(run_step);
		
		step_2.disableStep();
		
		StepPanel [] stepsPanels = {step_1, step_2, step_3};
		
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
		
		String criteria_example_file = "criteria_example_"+n+".csv";
		String knowledge_example_file = "knowledge_example_"+n+".csv";
		String cpref_rules_example_file = "cpref_rules_example_"+n+" ("+subject+").csv";
		
		File criteria_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+criteria_example_file);
		File knowledge_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+knowledge_example_file);
		File cpref_rules_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+cpref_rules_example_file);
		
		TableModel criteriaModel = new CriteriaTableModelBuilder(new CSVTableReader(criteria_file)).getTableModel();
		TableModel knowledgeModel = new EvidenceTableModelBuilder(new CSVTableReader(knowledge_file)).getTableModel();
		TableModel cprefRulesModel = new CPrefRulesTableModelBuilder(new CSVTableReader(cpref_rules_file)).getTableModel();
				
		step_1.setTableModel(criteriaModel);
		step_2.setTableModel(knowledgeModel);
		step_3.setTableModel(cprefRulesModel);
		
		step_2.enableStep();
		step_3.enableStep();
		run_step.enableStep();
	}
	
}

package java_ui;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import javax.swing.JSeparator;
import javax.swing.table.TableModel;

import java_ui.steps.DefineCprefRulesStepPanel;
import java_ui.steps.DefineEvidenceStepPanel;
import java_ui.steps.DefineStepPanel;
import java_ui.steps.RunStepPanel;
import java_ui.table_editor.model_builder.CriteriaTableModelBuilder;
import java_ui.table_editor.model_builder.EvidenceTableModelBuilder;
import java_ui.table_editor.model_builder.RulesTableModelBuilder;
import java_ui.table_editor.table_reader.CSVTableReader;

import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.awt.event.ActionEvent;
import java.awt.BorderLayout;

public class ExamplesLoadPanel extends JPanel {
	
	
	private DefineStepPanel criteriaStep;
	private DefineEvidenceStepPanel evidenceStep;
	private DefineCprefRulesStepPanel rulesStep;
	private RunStepPanel runStep;


	public ExamplesLoadPanel(DefineStepPanel criteriaStep, DefineEvidenceStepPanel evidenceStep, DefineCprefRulesStepPanel rulesStep, RunStepPanel runStep) {
		this.criteriaStep = criteriaStep;
		this.evidenceStep = evidenceStep;
		this.rulesStep = rulesStep;
		this.runStep = runStep;
		
		
		setLayout(new BorderLayout(0, 0));
		
		JPanel container = new JPanel();
		GridBagLayout gbc_container = new GridBagLayout();
		gbc_container.columnWidths = new int[]{0, 66, 118, 95, 0};
		gbc_container.rowHeights = new int[]{37, 39, 0, 0};
		gbc_container.columnWeights = new double[]{};
		gbc_container.rowWeights = new double[]{};
		add(container, BorderLayout.CENTER);
		GridBagLayout gbl_container = new GridBagLayout();
		gbl_container.columnWidths = new int[]{45, 110, 0};
		gbl_container.rowHeights = new int[]{70, 0};
		gbl_container.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		gbl_container.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		container.setLayout(gbl_container);
		
		JButton loadButton = new JButton("Load example");
		loadButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					loadExample();
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		});
		
		JLabel instructionLabel = new JLabel("Click on the button to load an example.");
		GridBagConstraints gbc_instructionLabel = new GridBagConstraints();
		gbc_instructionLabel.gridy = 0;
		gbc_instructionLabel.insets = new Insets(5, 5, 0, 5);
		gbc_instructionLabel.anchor = GridBagConstraints.WEST;
		gbc_instructionLabel.gridx = 0;
		container.add(instructionLabel, gbc_instructionLabel);
		GridBagConstraints gbc_loadButton = new GridBagConstraints();
		gbc_loadButton.anchor = GridBagConstraints.WEST;
		gbc_loadButton.insets = new Insets(0, 0, 0, 5);
		gbc_loadButton.gridx = 1;
		gbc_loadButton.gridy = 0;
		container.add(loadButton, gbc_loadButton);
		
		JSeparator separator = new JSeparator();
		add(separator, BorderLayout.SOUTH);
	
	}
	
	
	private void loadExample() throws IOException{
			
		
			String criteria_example_path = "criteria_example.csv";
			String evidence_example_path = "evidence_example.csv";
			String cpref_rules_example_path = "cpref_rules_example.csv";
			String rules_strength_example_path = "rules_strength_example.csv";
			
			
			File criteria_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+criteria_example_path);
			File evidence_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+evidence_example_path);
			File cpref_rules_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+cpref_rules_example_path);
			File rules_strength_file = new File(DSJavaUI.getExamplesFolderRelativePath()+"/examples/"+rules_strength_example_path);
			
			
			TableModel criteriaModel = new CriteriaTableModelBuilder(new CSVTableReader(criteria_file)).getTableModel();
			TableModel evidenceModel = new EvidenceTableModelBuilder(new CSVTableReader(evidence_file)).getTableModel();
			TableModel cprefRulesModel = new RulesTableModelBuilder(new CSVTableReader(cpref_rules_file)).getTableModel();
			
			
			criteriaStep.setTableModel(criteriaModel);
			evidenceStep.setTableModel(evidenceModel);
			rulesStep.setTableModel(cprefRulesModel);
			
			rulesStep.defineRulesStrenght(rules_strength_file);
			
			runStep.enableStep();
			
		}

}

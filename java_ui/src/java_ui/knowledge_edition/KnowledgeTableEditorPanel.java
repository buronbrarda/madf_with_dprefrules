package java_ui.knowledge_edition;

import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import java_ui.CSVTableReader;
import java_ui.TableEditorPanel;
import java_ui.TableModelBuilder;
import java_ui.cpref_rules_edition.DefineCPrefRuleDialog;
import java_ui.cpref_rules_edition.DefineCPrefRuleDialog.DefineCprefRuleEditingMode;

import java.awt.event.ActionEvent;

public class KnowledgeTableEditorPanel extends TableEditorPanel {
	
	public KnowledgeTableEditorPanel() throws IOException {
		this.init();
	}
	
	@Override
	protected void addButtonAction(ActionEvent event) {
		new DefineCPrefRuleDialog(DefineCprefRuleEditingMode.NEW,"r0");
	}

	@Override
	protected void editButtonAction(ActionEvent event) {
		String selectedRule = (String) table.getModel().getValueAt(table.getSelectedRow(), 0);
		new DefineCPrefRuleDialog(DefineCprefRuleEditingMode.EDIT,selectedRule);
	}

	@Override
	protected void removeButtonAction(ActionEvent event) {
		// TODO Auto-generated method stub
	}

	@Override
	protected void fileLoadButtonAction(ActionEvent event) throws IOException {
		File f = loadCSVFile();
		
		if(f != null) {
			TableModelBuilder tmb = new EvidenceTableModelBuilder(new CSVTableReader(f));
			
			this.setTableModel(tmb.getTableModel());
		}
		
	}
	
	
	private File loadCSVFile(){
		
		JFileChooser fc = new JFileChooser("./");
		fc.setDialogTitle("Select the file to load");
		
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fc.setFileFilter(new FileNameExtensionFilter("CSV File", "csv"));
		fc.setMultiSelectionEnabled(false);
		
		File toReturn = null;
		
		if(fc.showDialog(null, "Load") == JFileChooser.APPROVE_OPTION){
			toReturn = fc.getSelectedFile();
		}
		
		
		return toReturn;
	}
	

}

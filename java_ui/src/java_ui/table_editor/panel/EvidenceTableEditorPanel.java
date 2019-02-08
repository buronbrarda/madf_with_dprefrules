package java_ui.table_editor.panel;

import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import java_ui.DSJavaUI;
import java_ui.DefineCPrefRuleDialog;
import java_ui.DefineCPrefRuleDialog.DefineCprefRuleEditingMode;
import java_ui.table_editor.model_builder.EvidenceTableModelBuilder;
import java_ui.table_editor.model_builder.TableModelBuilder;
import java_ui.table_editor.table_reader.CSVTableReader;

import java.awt.event.ActionEvent;

public class EvidenceTableEditorPanel extends TableEditorPanel {
	
	public EvidenceTableEditorPanel() throws IOException {
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
	protected String fileLoadButtonAction(ActionEvent event) throws IOException {
		File f = loadCSVFile();
		
		String filePath = null;
		
		if(f != null) {
			TableModelBuilder tmb = new EvidenceTableModelBuilder(new CSVTableReader(f));
			
			this.setTableModel(tmb.getTableModel());
			
			filePath  = f.getAbsolutePath();
		}
		
		return filePath;
		
	}
	
	
	private File loadCSVFile(){
		
		JFileChooser fc = new JFileChooser(DSJavaUI.getExamplesFolderRelativePath());
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

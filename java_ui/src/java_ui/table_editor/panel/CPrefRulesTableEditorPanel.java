package java_ui.table_editor.panel;

import java.io.File;
import java.io.IOException;

import java_ui.DefineCPrefRuleDialog;
import java_ui.DefineCPrefRuleDialog.DefineCprefRuleEditingMode;
import java_ui.table_editor.model_builder.RulesTableModelBuilder;
import java_ui.table_editor.model_builder.TableModelBuilder;
import java_ui.table_editor.table_reader.CSVTableReader;

import java.awt.event.ActionEvent;

public class CPrefRulesTableEditorPanel extends TableEditorPanel {
	
	public CPrefRulesTableEditorPanel() throws IOException {
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
		
		String filePath = null;
		
		File f = this.loadCSVFile();
		
		
		if(f != null) {
			TableModelBuilder tmb = new RulesTableModelBuilder(new CSVTableReader(f));
			
			this.setTableModel(tmb.getTableModel());
			
			filePath = f.getAbsolutePath();
		}
		
		return filePath;
		
	}
	

}

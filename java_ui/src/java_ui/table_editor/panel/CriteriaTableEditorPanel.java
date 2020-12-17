package java_ui.table_editor.panel;

import java.io.File;
import java.io.IOException;

import java_ui.table_editor.model_builder.CriteriaTableModelBuilder;
import java_ui.table_editor.model_builder.TableModelBuilder;
import java_ui.table_editor.table_reader.CSVTableReader;

import java.awt.event.ActionEvent;

public class CriteriaTableEditorPanel extends TableEditorPanel {
	
	public CriteriaTableEditorPanel() throws IOException {
		this.init();
	}
	
	@Override
	protected void addButtonAction(ActionEvent event) {
		// TODO Auto-generated method stub
	}

	@Override
	protected void editButtonAction(ActionEvent event) {
		// TODO Auto-generated method stub
	}

	@Override
	protected void removeButtonAction(ActionEvent event) {
		// TODO Auto-generated method stub
	}

	@Override
	protected String fileLoadButtonAction(ActionEvent event) throws IOException {
		File f = this.loadCSVFile();
		
		String filePath = null;
		
		if(f != null) {
			TableModelBuilder tmb = new CriteriaTableModelBuilder(new CSVTableReader(f));
			
			this.setTableModel(tmb.getTableModel());
			
			filePath = f.getAbsolutePath();
		}
		
		return filePath;
		
	}

}

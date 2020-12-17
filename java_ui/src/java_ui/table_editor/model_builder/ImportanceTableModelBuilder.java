package java_ui.table_editor.model_builder;

import java.io.IOException;

import javax.swing.table.DefaultTableModel;

import java_ui.table_editor.table_reader.CSVTableReader;

public class ImportanceTableModelBuilder implements TableModelBuilder{
	
	private CSVTableReader reader;
	private final String [] headers = {"Agent", "Importance Order"};

	public ImportanceTableModelBuilder(CSVTableReader reader) {
		this.reader = reader;
	}
	
	@Override
	public DefaultTableModel getTableModel(){
		
		DefaultTableModel toReturn = new DefaultTableModel(this.headers,0);
		
		while(this.reader.hasNext()){
			toReturn.addRow(reader.next());
		}
		
		try {
			this.reader.closeFile();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return toReturn;
	}
}

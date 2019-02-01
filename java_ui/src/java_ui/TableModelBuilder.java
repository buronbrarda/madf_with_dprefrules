package java_ui;

import javax.swing.table.DefaultTableModel;

public class TableModelBuilder {

	
	public static DefaultTableModel buildTable(CSVReader reader){
		
		DefaultTableModel toReturn = new DefaultTableModel(reader.getHeaders(),0);
		
		while(reader.hasNext()){
			toReturn.addRow(reader.next());
		}
		
		return toReturn;
	}
}

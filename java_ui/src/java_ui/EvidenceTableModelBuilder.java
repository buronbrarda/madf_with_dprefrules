package java_ui;

import javax.swing.table.DefaultTableModel;

public class EvidenceTableModelBuilder implements TableModelBuilder{
	
	private CSVTableReader reader;

	public EvidenceTableModelBuilder(CSVTableReader reader) {
		this.reader = reader;
	}
	
	@Override
	public DefaultTableModel getTableModel(){
		
		DefaultTableModel toReturn = new DefaultTableModel(this.reader.getHeaders(),0);
		
		while(this.reader.hasNext()){
			toReturn.addRow(reader.next());
		}
		
		return toReturn;
	}
}

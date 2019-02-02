package java_ui;

import javax.swing.table.DefaultTableModel;

public class CriteriaTableModelBuilder implements TableModelBuilder{
	
	private CSVTableReader reader;
	private final String [] headers = {"Criterion", "Domain (Assessment Values)"};

	public CriteriaTableModelBuilder(CSVTableReader reader) {
		this.reader = reader;
	}
	
	@Override
	public DefaultTableModel getTableModel(){
		
		DefaultTableModel toReturn = new DefaultTableModel(this.headers,0);
		
		while(this.reader.hasNext()){
			toReturn.addRow(reader.next());
		}
		
		return toReturn;
	}
}

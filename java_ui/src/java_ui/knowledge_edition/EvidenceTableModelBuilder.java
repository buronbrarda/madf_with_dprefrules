package java_ui.knowledge_edition;

import java.io.IOException;

import javax.swing.table.DefaultTableModel;

import java_ui.CSVTableReader;
import java_ui.TableModelBuilder;

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
		
		try {
			this.reader.closeFile();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return toReturn;
	}
}

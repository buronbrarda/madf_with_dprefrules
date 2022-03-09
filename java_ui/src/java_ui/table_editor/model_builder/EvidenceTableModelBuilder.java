package java_ui.table_editor.model_builder;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

import javax.swing.table.DefaultTableModel;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import java_ui.table_editor.table_reader.CSVTableReader;

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
	
	static public DefaultTableModel loadEvidence(){
		
		Query q = new Query("criterion(C,_)");
		
		ArrayList<String> columns = new ArrayList<String>();
		columns.add("alternative");
		
		while(q.hasNext()){
			Map<String, Term> s = q.next();
			
			columns.add(s.get("C").toString());
		}
		
		DefaultTableModel toReturn = new DefaultTableModel(columns.toArray(), 0);
		
		Query q2 = new Query("alternative(A),findall(Pair,criteria_value_pair(A,Pair),Values)");
		
		ArrayList<ArrayList<String>> rows = new ArrayList<ArrayList<String>>();
		
		while(q2.hasNext()){
			ArrayList<String> row = new ArrayList<String>(columns.size());
			//Ensure list's size is correct.
			for(int i = 0; i<columns.size();i++) {
				row.add(null);
			}
			
			Map<String, Term> s = q2.next();
			
			row.set(0,s.get("A").toString());
			
			
			Term[] pairList = Util.listToTermArray(s.get("Values"));
			
			for(Term pair : pairList){
				Term[] aux = Util.listToTermArray(pair);
				
				int c_index = columns.indexOf(aux[0].toString());
				String value = aux[1].toString();
				
				row.set(c_index, value);
			}
			
			rows.add(row);
		}
		
		
		for(ArrayList<String> r : rows){
			toReturn.addRow(r.toArray());
		}
		
		return toReturn;
		
	}
}

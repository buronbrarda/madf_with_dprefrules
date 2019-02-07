package java_ui.prolog_loader;

import javax.swing.table.TableModel;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class KnowledgePrologLoader implements PrologLoader{

	private TableModel tm;
	private String err_msg;
	private PrologLoader.StatusCode status;
	
	
	private String getAlternative(TableModel tm, int row) {
		return (String) tm.getValueAt(row, 0);
	}
	
	private String [] getCriteria(TableModel tm) {
		
		String [] toReturn = new String [tm.getColumnCount()-1];
		
		for(int i = 1; i < tm.getColumnCount(); i++){
			toReturn[i-1] = tm.getColumnName(i);
		}
		
		return toReturn;
	}
	
	private String getValue(TableModel tm, int row, int column) {
		return (String) tm.getValueAt(row, column);
	}
	
	
	private void loadAssessment(String alternative, String values) throws PrologLoadException {
		Query q = new Query("add_assessed_alternative", new Term [] {new Atom(alternative),  Util.textToTerm(values)});
		
		if(!q.hasSolution()) {
			
			this.err_msg = "There was a problem while loading alternative '"+alternative+"'."+"\n"
					+ "Please, check if the associated criteria and their values are correct."+"\n"
					+ "Values = "+values+".";
			
			this.status = PrologLoader.StatusCode.Error;
			
			
			throw new PrologLoadException(getErrorMessage());
		}else{
			
			this.status = PrologLoader.StatusCode.Ok;
		}
	}
	
	@Override
	public void loadData(TableModel tm) throws PrologLoadException{
		
		this.tm = tm;
		
		String alternative, values;
		String [] criteria = getCriteria(this.tm);
		
		int i,j;
		
		cleanKnowledgeBase();
		
		for(i = 0; i < tm.getRowCount(); i++) {
			
			alternative = getAlternative(tm, i);
			
			values = "[";
			
			for(j = 0; j < criteria.length-1; j++) {
				values = values + "["+criteria[j]+","+getValue(this.tm,i,j+1)+"],";
			}
			
			values = values + "["+criteria[j]+","+getValue(this.tm,i,j+1)+"]]";
			
			loadAssessment(alternative, values);
		}
	}
	
	
	private void cleanKnowledgeBase() {
		Query q = new Query("remove_alternatives");
		q.hasSolution();
	}


	@Override
	public PrologLoader.StatusCode getStatus() {
		return this.status;
	}


	@Override
	public String getErrorMessage() {
		return this.err_msg;
	}
	
	
	
}

package java_ui;

import java.util.Map;

import javax.swing.table.TableModel;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class CriteriaPrologLoader implements PrologLoader{

	private TableModel tm;
	private String err_msg;
	private PrologLoader.StatusCode status;
	
	
	private String getCriterion(int row) {
		return (String) tm.getValueAt(row, 0);
	}
	
	private String getValues(int row) {
		return (String) tm.getValueAt(row, 1);
	}
	
	
	private void loadCriterion(String criterion, String values) throws PrologLoadException {	
		
		Query q = new Query("add_pair", new Term [] {new Atom(criterion), Util.textToTerm(values)});
		
		
		if(!q.hasSolution()) {
			
			this.err_msg = "There was a problem while loading criterion '"+criterion+"'."
					+ "Please, check if the associated values can be unified with a prolog-list."
					+ "Values = "+values+".";
			
			this.status = PrologLoader.StatusCode.Error;
			
			
			throw new PrologLoadException(getErrorMessage());
		};
		
	}
	
	@Override
	public void loadData(TableModel tm) throws PrologLoadException{
		
		this.tm = tm;
		
		String criterion, values;
		
		//row(0) = headers
		for(int i = 1; i < tm.getRowCount(); i++) {
			
			criterion = getCriterion(i);
			values = getValues(i);
			
			loadCriterion(criterion, values);
		}
		
		
		Query test = new Query("values(C,V)");
		Map<String, Term> solution;
		
		while(test.hasNext()){
			solution = test.next();
			System.out.println("C = "+solution.get("C")+"; V = "+solution.get("V"));
		}
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

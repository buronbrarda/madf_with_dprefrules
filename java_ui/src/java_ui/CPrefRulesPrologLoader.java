package java_ui;

import javax.swing.table.TableModel;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;

public class CPrefRulesPrologLoader implements PrologLoader{

	private TableModel tm;
	private PrologLoader.StatusCode status;
	private String err_message;
	
	
	private String getId(int row) {
		return (String) tm.getValueAt(row, 0);
	}
	
	private String getRule(int row) {
		return (String) tm.getValueAt(row, 1);
	}
	
	
	
	private void loadRule(String id, String rule) throws PrologLoadException {
		Query q = new Query("add_cpref_rule", new Term [] {new Atom(id), new Atom(rule)});
		
		if(!q.hasSolution()) {
			
			this.err_message = "There was a problem while loading rule '"+id+"'."
					+ "Please, check if its syntax is correct and if it is a coherent CPref-Rule.";
			
			this.status = PrologLoader.StatusCode.Error;
			
			throw new PrologLoadException(getErrorMessage());
		};
	}
	
	@Override
	public void loadData(TableModel tm) throws PrologLoadException{
		this.tm = tm;
		
		String id, rule;
		
		//row(0) = headers
		for(int i = 1; i < tm.getRowCount(); i++) {
			
			id = getId(i);
			rule = getRule(i);
			
			loadRule(id, rule);
		}
		
		this.status = PrologLoader.StatusCode.Ok;
	}


	@Override
	public PrologLoader.StatusCode getStatus() {
		return this.status;
	}


	@Override
	public String getErrorMessage() {
		return this.err_message;
	}
	
	
	
}

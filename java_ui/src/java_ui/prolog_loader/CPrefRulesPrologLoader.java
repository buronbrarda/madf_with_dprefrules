package java_ui.prolog_loader;

import javax.swing.table.TableModel;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

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
		
		String[] splittedRule = rule.split("==>");
		
		Query q = new Query("add_cpref_rule", new Term [] {new Atom(id), new Compound("==>", new Term [] {Util.textToTerm(splittedRule[0]), Util.textToTerm(splittedRule[1])})});
		
		if(!q.hasSolution()) {
			
			this.err_message = "There was a problem while loading rule '"+id+"'."+"\n"
					+ "Please, check if its syntax is correct and if it is a coherent CPref-Rule.";
			
			this.status = PrologLoader.StatusCode.Error;
			
			throw new PrologLoadException(getErrorMessage());
		}else{
				
			this.status = PrologLoader.StatusCode.Ok;
		}
	}
	
	@Override
	public void loadData(TableModel tm) throws PrologLoadException{
		this.tm = tm;
		
		String id, rule;
		
		cleanCPrefRules();
		
		for(int i = 0; i < tm.getRowCount(); i++) {
			
			id = getId(i);
			rule = getRule(i);
			
			loadRule(id, rule);
		}
		
		this.status = PrologLoader.StatusCode.Ok;
	}

	
	private void cleanCPrefRules(){
		Query q = new Query("remove_cpref_rules");
		q.hasSolution();
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

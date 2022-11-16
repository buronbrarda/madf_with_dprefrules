package java_ui.prolog_loader;

import javax.swing.table.TableModel;

import org.jpl7.Query;

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
		
		Query q = new Query("add_cpref_rule("+id+",("+rule+"))");
		
		if(!q.hasSolution()) {
			
			this.err_message = "There was a problem while loading rule '"+id+"'."+"\n"
					+ "Please, check if its syntax is correct and if it is coherent.";
			
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

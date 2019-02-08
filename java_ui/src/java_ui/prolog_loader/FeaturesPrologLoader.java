package java_ui.prolog_loader;

import javax.swing.table.TableModel;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

public class FeaturesPrologLoader implements PrologLoader{

	private TableModel tm;
	private String err_msg;
	private PrologLoader.StatusCode status;
	
	
	private String getFeature(int row) {
		return (String) tm.getValueAt(row, 0);
	}
	
	private String getDomain(int row) {
		return (String) tm.getValueAt(row, 1);
	}
	
	
	private void loadCriterion(String feature, String domain) throws PrologLoadException {
		
		Query q = new Query("add_feature", new Term [] {new Atom(feature), Util.textToTerm(domain)});
		
		
		if(!q.hasSolution()) {
			
			this.err_msg = "There was a problem while loading criterion '"+feature+"'."+"\n"
					+ "Please, check criterion duplication and check if the associated domain "+"\n"
					+ "can be unify with a prolog-list; or with 'real_numbers'"+"\n"
					+ "domain = "+domain+".";
			
			this.status = PrologLoader.StatusCode.Error;
			
			
			throw new PrologLoadException(getErrorMessage());
		}
		else{
			this.status = PrologLoader.StatusCode.Ok;
		}
		
	}
	
	@Override
	public void loadData(TableModel tm) throws PrologLoadException{
		
		this.tm = tm;
		
		String criterion, values;
		
		cleanCriteria();
		

		for(int i = 0; i < tm.getRowCount(); i++) {
			
			criterion = getFeature(i);
			values = getDomain(i);
			
			loadCriterion(criterion, values);
		}
	}


	private void cleanCriteria() {
		Query q = new Query("remove_features");
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

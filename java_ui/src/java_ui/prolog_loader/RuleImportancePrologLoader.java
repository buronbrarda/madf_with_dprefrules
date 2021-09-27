package java_ui.prolog_loader;

import java.io.File;
import java.io.IOException;

import javax.swing.table.TableModel;

import org.jpl7.Query;

import java_ui.table_editor.table_reader.CSVTableReader;

public class RuleImportancePrologLoader implements PrologLoader{

	private TableModel tm;
	private PrologLoader.StatusCode status;
	private String err_message;
	
	
	private String getAgent(int row) {
		return (String) tm.getValueAt(row, 0);
	}
	
	private String getOrder(int agent) {
		return (String) tm.getValueAt(agent, 1);
	}
	
	
	
	private void setAgentImportanceOrder(String agent, String order) throws PrologLoadException {
		
		
		for(String statement : order.trim().split(",")) {
			Query q = new Query("add_importance_statement("+agent+",("+statement+"))");
		
			if(!q.hasSolution()) {
				
				this.err_message = "There was a problem while loading the importace order for agent '"+agent+"', statement: '"+statement+"'."+"\n"
						+ "Please, check if its syntax is correct and if it is a coherent CPref-Rule.";
				
				this.status = PrologLoader.StatusCode.Error;
				
				throw new PrologLoadException(getErrorMessage());
			}else{
					
				this.status = PrologLoader.StatusCode.Ok;
			}
		}
	}
	
	@Override
	public void loadData(TableModel tm) throws PrologLoadException{
		this.tm = tm;
		
		String agent, order;
		
		cleanImportanceOrders();
		
		for(int i = 0; i < tm.getRowCount(); i++) {
			
			agent = getAgent(i);
			order = getOrder(i);
			
			setAgentImportanceOrder(agent, order);
		}
		
		this.status = PrologLoader.StatusCode.Ok;
	}

	
	private void cleanImportanceOrders(){
		Query q = new Query("remove_importance_orders");
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
	
	
	public void setAgentPriorityOrder(File file) {
		try {
			Query q = new Query("remove_priorities");
			q.hasSolution(); q.close();
			
			
			CSVTableReader reader = new CSVTableReader(file);
			
			for(String[] row : reader) {
				q = new Query("add_priority("+row[0]+")");
				while(q.hasNext()) {q.next();}
			}
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
	}
	
	
}

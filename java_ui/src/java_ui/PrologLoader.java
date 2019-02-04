package java_ui;

import javax.swing.table.TableModel;

public interface PrologLoader {
	
	public static enum StatusCode {Ok,Error};
	
	public void loadData(TableModel tm) throws PrologLoadException;
	
	public StatusCode getStatus();
	
	public String getErrorMessage();
}

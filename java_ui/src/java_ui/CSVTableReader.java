package java_ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;

public class CSVTableReader implements Iterator<String[]>{
	
	private BufferedReader reader = null;
	private String line = null;
	private String[] headers = null;
	private String splitter = ";";
	
	
    public CSVTableReader(File file) throws IOException {    		
    	this.readFile(file);
	}
    
    public CSVTableReader(File file, String splitter) throws IOException {    		
    	this.splitter = splitter;
    	
    	this.readFile(file);
	}
    
    private void readFile(File file) throws IOException{
    	this.reader = new BufferedReader(new FileReader(file));
    	
    	this.line  = this.reader.readLine();
    	
    	if(this.line != null && this.line.length() > 0){
    		this.headers = this.line.split(this.splitter);
    	}
    }
    
    public void closeFile() throws IOException{
    	if(this.reader != null){
    		this.reader.close();
    		this.reader = null;
    	}
    }
    
    public String[] getHeaders(){
    	return this.headers;
    }
    
    
    @Override
    protected void finalize() throws Throwable {
    	this.closeFile();
    	super.finalize();
    }
    
	@Override
	public boolean hasNext() {
		try {
			do{
				this.line  = this.reader.readLine();
			}while(line != null && line.length() <= 0);
			
			return this.reader != null && this.line != null;
		
		} catch (IOException e) {

			e.printStackTrace();
			return false;
		}
	}

	@Override
	public String[] next() {
		return this.line.split(this.splitter);
	}

}
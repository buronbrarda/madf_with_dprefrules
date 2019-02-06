package java_ui;

import java.awt.EventQueue;
import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.BorderLayout;

import org.jpl7.*;

public class DSJavaUI{

	private JFrame frame;
	
	private static final boolean productionFlag = false;
	
	private static final String productionPrologFilesPath = "./swipl_core/";
	private static final String prologFilesPath = "../swipl_core/";
	private static final String prologEntryPoint = "decision_framework.pl";
	

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					DSJavaUI window = new DSJavaUI();
					window.frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public DSJavaUI() {
		initialize();
		
		JPanel stepsView;
		try {
			stepsView = new AllStepsPanel();
		
		frame.getContentPane().add(stepsView, BorderLayout.CENTER);
		
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setBounds(100, 100, 450, 300);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		
		Atom filesPath;
		
		if(DSJavaUI.productionFlag){
			filesPath = new Atom(DSJavaUI.productionPrologFilesPath + DSJavaUI.prologEntryPoint);
		}
		else{
			filesPath = new Atom(DSJavaUI.prologFilesPath + DSJavaUI.prologEntryPoint);
		}
		
		new Query("consult", new Term [] {filesPath}).hasSolution();
	}
	
	
	public static String getExamplesFolderRelativePath(){
		if(DSJavaUI.productionFlag){
			return "./";
		}
		else{
			return "../";
		}
	}

}
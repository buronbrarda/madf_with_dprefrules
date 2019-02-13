package java_ui;

import java.awt.EventQueue;
import java.io.IOException;
import javax.swing.JFrame;
import org.jpl7.*;

import java_ui.steps.ResultsPanel;

import javax.swing.BoxLayout;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

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
		
		AllStepsPanel stepsView;
		ResultsPanel resultsView;
		
		try {
			
			resultsView = new ResultsPanel();
			stepsView = new AllStepsPanel(resultsView);
			
			frame.getContentPane().setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.X_AXIS));
			
			JSeparator separator_2 = new JSeparator();
			separator_2.setOrientation(SwingConstants.VERTICAL);
			frame.getContentPane().add(separator_2);
			
			
			frame.getContentPane().add(stepsView);
			
			JSeparator separator = new JSeparator();
			separator.setOrientation(SwingConstants.VERTICAL);
			frame.getContentPane().add(separator);
			
			
			frame.getContentPane().add(resultsView);
			
			JSeparator separator_1 = new JSeparator();
			separator_1.setOrientation(SwingConstants.VERTICAL);
			frame.getContentPane().add(separator_1);
		
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setBounds(100, 100, 857, 566);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		frame.setTitle("Argumentation-Based Multi-Criteria Decision Support System With Conditional Preferences");
		
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
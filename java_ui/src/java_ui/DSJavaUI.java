package java_ui;

import java.awt.EventQueue;
import java.io.IOException;
import javax.swing.JFrame;
import org.jpl7.*;

import java_ui.steps.ResultsPanel;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

public class DSJavaUI{

	private JFrame frame;
	
	private static final boolean productionFlag = false;
	
	private static final String productionPrologFilesPath = "./swipl_core/";
	private static final String prologFilesPath = "../../swipl_core/";
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
		
		
		ResultsPanel resultsView = new ResultsPanel();
		AllStepsPanel stepsView;
		
		try {
			frame.getContentPane().setLayout(new BorderLayout(0, 0));
			
			JPanel panel = new JPanel();
			frame.getContentPane().add(panel);
			GridBagLayout gbl_panel = new GridBagLayout();
			gbl_panel.columnWidths = new int[]{450, 10, 470, 0};
			gbl_panel.rowHeights = new int[]{562, 0};
			gbl_panel.columnWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
			gbl_panel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
			panel.setLayout(gbl_panel);
			stepsView = new AllStepsPanel(resultsView);
			
			GridBagConstraints gbc_stepsView = new GridBagConstraints();
			gbc_stepsView.fill = GridBagConstraints.BOTH;
			gbc_stepsView.insets = new Insets(0, 0, 0, 5);
			gbc_stepsView.gridx = 0;
			gbc_stepsView.gridy = 0;
			panel.add(stepsView, gbc_stepsView);
			{
				JSeparator separator = new JSeparator();
				separator.setOrientation(SwingConstants.VERTICAL);
				GridBagConstraints gbc_separator = new GridBagConstraints();
				gbc_separator.fill = GridBagConstraints.VERTICAL;
				gbc_separator.insets = new Insets(0, 0, 0, 5);
				gbc_separator.gridx = 1;
				gbc_separator.gridy = 0;
				panel.add(separator, gbc_separator);
			}
			
			GridBagConstraints gbc_resultsView = new GridBagConstraints();
			gbc_resultsView.fill = GridBagConstraints.BOTH;
			gbc_resultsView.gridx = 2;
			gbc_resultsView.gridy = 0;
			panel.add(resultsView, gbc_resultsView);
		
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setResizable(false);
		frame.setBounds(100, 100, 1220, 526);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		frame.setTitle("Multi-agent Argumentation-based Multi-criteria Framework for Decision-making with Defeasible Preferences - (Prototype Version)");
		
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
			return "../../";
		}
	}

}
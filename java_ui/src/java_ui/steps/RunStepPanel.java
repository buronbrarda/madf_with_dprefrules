package java_ui.steps;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JOptionPane;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import java.awt.GridBagLayout;
import java.awt.Cursor;
import java.awt.GridBagConstraints;
import java.awt.Insets;

public class RunStepPanel extends StepPanel {
	
	private JButton runButton;
	private ResultsPanel resultsPanel;
	
	public RunStepPanel(StepPanel first_step, ResultsPanel resultsPanel){
		
		this.resultsPanel = resultsPanel;
		
		//The run step must be the last step.
		this.setFollowingStep(null);
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{99, 285, 87, 0};
		gridBagLayout.rowHeights = new int[]{23, 0};
		gridBagLayout.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JButton clearButton = new JButton("Clear");
		clearButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				first_step.cleanStep();
			}
		});
		GridBagConstraints gbc_clearButton = new GridBagConstraints();
		gbc_clearButton.insets = new Insets(5, 5, 5, 5);
		gbc_clearButton.gridx = 0;
		gbc_clearButton.gridy = 0;
		add(clearButton, gbc_clearButton);
		
		this.runButton = new JButton("Run");
		GridBagConstraints gbc_runButton = new GridBagConstraints();
		gbc_runButton.insets = new Insets(5, 5, 5, 5);
		gbc_runButton.gridx = 2;
		gbc_runButton.gridy = 0;
		this.add(runButton, gbc_runButton);
		
		runButton.addActionListener(new ActionListener(){

			@Override
			public void actionPerformed(ActionEvent e) {
				 runButtonAction(e);
			}
			
		});
	}
	
	@Override
	public void enableStep() {
		this.runButton.setEnabled(true);
		
		resultsPanel.setSelectedAlternatives("");
		resultsPanel.setAlternativesRelation("");
	}

	@Override
	public void disableStepAction() {
		this.runButton.setEnabled(false);
		
		resultsPanel.setSelectedAlternatives("");
		resultsPanel.setAlternativesRelation("");
		
		resultsPanel.setArgumentsCount("");
		resultsPanel.setReasoningTime("");
		resultsPanel.setSelectionTime("");
		
		resultsPanel.disableGraphButtons();
	}
	
	
	private void runButtonAction(ActionEvent e){
		this.cleanStep();
		
		Query q = new Query("run(Selection,Order,Args_Count,Reasoning_Time,Selection_Time)");
		
		Map<String, Term> solution;
		
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		
		while(q.hasNext()){
			solution = q.next();
			resultsPanel.setSelectedAlternatives(termArrayToText(Util.listToTermArray(solution.get("Selection"))));
			resultsPanel.setAlternativesRelation(parseAlternativesRelation(Util.listToTermArray(solution.get("Order"))));
			resultsPanel.setArgumentsCount(solution.get("Args_Count").toString());
			resultsPanel.setReasoningTime(solution.get("Reasoning_Time").toString()+" ms");
			resultsPanel.setSelectionTime(solution.get("Selection_Time").toString()+" ms");
			resultsPanel.enableGraphButtons();
		}
		
		if(!q.hasSolution()){
			String message = "The completeness and consistency requirements for the assessments base are not fullfilled.\n"+
							 "Please, check if the set if set profile rules and the evidence set are correct.";
			
			JOptionPane.showMessageDialog(null, message, "Error", JOptionPane.ERROR_MESSAGE);
		}
		
		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		
		resultsPanel.loadAlternativesGraph();
	}
	
	
	private String termArrayToText(Term [] list){
		String toReturn = "";
		
		int i;
		
		for(i = 0; i < list.length-1; i++){
			toReturn += list[i]+", ";
		}
		
		toReturn += list[i];
		
		return toReturn;
	}
	
	
	private String parseAlternativesRelation(Term [] list){
		String toReturn = "";
		
		int i;
		
		for(i = 0; i < list.length-1; i++){
			toReturn += "["+termArrayToText(Util.listToTermArray(list[i]))+"] --> ";
		}
		
		toReturn += "["+termArrayToText(Util.listToTermArray(list[i]))+"]";
		
		return toReturn;
	}

	@Override
	protected void cleanStepAction() {
		resultsPanel.cleanGraphs();
		
		resultsPanel.setSelectedAlternatives("");
		resultsPanel.setAlternativesRelation("");
		resultsPanel.setArgumentsCount("");
		resultsPanel.setReasoningTime("");
		resultsPanel.setSelectionTime("");
		
		resultsPanel.disableGraphButtons();
	}
}

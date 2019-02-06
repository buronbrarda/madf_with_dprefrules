package java_ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;

import javax.swing.JButton;

import org.jpl7.Query;
import org.jpl7.Term;

public class RunStepPanel extends StepPanel {
	
	private JButton runButton;
	private ResultsPanel resultsPanel;
	
	RunStepPanel(ResultsPanel resultsPanel){
		
		this.resultsPanel = resultsPanel;
		
		//The run step must be the last step.
		this.setFollowingStep(null);
		
		this.runButton = new JButton("Run");
		this.add(runButton);
		
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
	}

	@Override
	public void disableStepAction() {
		this.runButton.setEnabled(false);
	}
	
	
	private void runButtonAction(ActionEvent e){
		Query q = new Query("run(Selection,Order)");
		
		Map<String, Term> solution;
		
		if(q.hasSolution()){
			q.open();
			solution = q.getSolution();
			resultsPanel.setSelectedAlternatives(solution.get("Selection").toString());
			resultsPanel.setAlternativesRelation(solution.get("Order").toString());
		}
	}

}

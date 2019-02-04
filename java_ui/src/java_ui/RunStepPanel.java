package java_ui;

import javax.swing.JButton;

public class RunStepPanel extends StepPanel {
	
	private JButton runButton;
	
	RunStepPanel(){
		//The run step must be the last step.
		this.setFollowingStep(null);
		
		this.runButton = new JButton("Run");
		this.add(runButton);
	}
	
	@Override
	public void enableStep() {
		this.runButton.setEnabled(true);
	}

	@Override
	public void disableStepAction() {
		this.runButton.setEnabled(false);
	}

}

package java_ui;

import javax.swing.JPanel;

public abstract class  StepPanel extends JPanel{
	
	private StepPanel following;
	
	public abstract void enableStep();
	
	public abstract void disableStepAction();
	
	public final void disableStep(){
		disableStepAction();
		
		if(this.following != null)
			this.following.disableStep();
	}
	
	public final StepPanel getFollowingStep(){
		return this.following;
	}
	
	public final void setFollowingStep(StepPanel fp){
		this.following = fp;
	}
}

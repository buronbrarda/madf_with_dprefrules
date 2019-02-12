package java_ui.steps;

import javax.swing.JPanel;

public abstract class  StepPanel extends JPanel{
	
	private StepPanel following;
	
	public abstract void enableStep();
	
	protected abstract void disableStepAction();
	
	protected abstract void cleanStepAction();
	
	public final void cleanStep(){
		cleanStepAction();
		
		if(this.following != null){
			this.following.cleanStep();
			this.following.disableStep();	
		}
	}
	
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

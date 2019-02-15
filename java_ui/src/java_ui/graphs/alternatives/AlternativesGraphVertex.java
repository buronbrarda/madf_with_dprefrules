package java_ui.graphs.alternatives;

import java.util.ArrayList;

public interface AlternativesGraphVertex {
	
	public String getId();
		
	public boolean isSelected();

	public ArrayList<String> getJustificationRulesFor(AlternativesGraphVertex v2);
}

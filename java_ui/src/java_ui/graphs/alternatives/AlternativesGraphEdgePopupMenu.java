package java_ui.graphs.alternatives;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Map;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import java_ui.arguments.Argument;
import java_ui.arguments.dialectical_trees.DeltaExplanationDialog;

public class AlternativesGraphEdgePopupMenu extends JPopupMenu{
	
	private AlternativesGraphEdge edge;
	
	public AlternativesGraphEdgePopupMenu(AlternativesGraph graph) {
		
		super();
		
		JMenuItem expandItem = new JMenuItem("Explain");
		expandItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				AlternativesGraphVertex v1 = graph.getGraph().getSource(edge);
				AlternativesGraphVertex v2 = graph.getGraph().getDest(edge);
				
				String claim = "pref("+v1.getId()+","+v2.getId()+")";
				
				Query q = new Query("justification("+claim+",Claim_U_Trees, NoClaim_U_Trees, BothClaim_D_Trees)");
				
				if(q.hasNext()){
					
					Map<String, Term> s = q.next();
					
					ArrayList<Argument> list = getArgumentsFromDtreeList(Util.listToTermArray(s.get("Claim_U_Trees")));
					
					list.addAll(getArgumentsFromDtreeList(Util.listToTermArray(s.get("NoClaim_U_Trees"))));
					
					list.addAll(getArgumentsFromDtreeList(Util.listToTermArray(s.get("BothClaim_D_Trees"))));
					
					new DeltaExplanationDialog(list).setVisible(true);
				}
				
			}
		});
		
		this.add(expandItem);
		
	}
	
	public void setEdge(AlternativesGraphEdge e){
		this.edge = e;
	}
	
	
	private ArrayList<Argument> getArgumentsFromDtreeList(Term [] list){
		
		ArrayList<Argument> arguments = new ArrayList<Argument>();
		
		for(Term t : list){
			
			Query q = new Query("dtree_node("+t.toString()+",null,_,Arg_Id,_)");
			
			if(q.hasNext()){
				Map<String, Term> s = q.next();
				
				Argument arg = new Argument(s.get("Arg_Id").toString());
				
				arguments.add(arg);
			}
			
		}
		
		return arguments;
	}
	
	
}

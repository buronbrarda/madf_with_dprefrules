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
import java_ui.arguments.dialectical_trees.ArgumentsGraphDialog;
import java_ui.arguments.dialectical_trees.DeltaExplanationDialog;
import java_ui.prolog_loader.PrologLoadException;

public class AlternativesGraphEdgePopupMenu extends JPopupMenu{
	
	private AlternativesGraphEdge edge;
	
	public AlternativesGraphEdgePopupMenu(final AlternativesGraph graph) {
		
		super();
		
		JMenuItem expandItem = new JMenuItem("Explain");
		expandItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				AlternativesGraphVertex v1 = graph.getGraph().getSource(edge);
				AlternativesGraphVertex v2 = graph.getGraph().getDest(edge);

				
				String claim = "pref("+v1.getId()+","+v2.getId()+")";
				
				Query q = new Query("justification("+claim+",Claim_U_Trees, NoClaim_U_Trees, BothClaim_D_Trees)");
				ArrayList<Argument> list = null;
				
				if(q.hasNext()){
					
					Map<String, Term> s = q.next();
					
					try{
						list = getArgumentsFromDtreeList(Util.listToTermArray(s.get("Claim_U_Trees")));
						
						list.addAll(getArgumentsFromDtreeList(Util.listToTermArray(s.get("NoClaim_U_Trees"))));
						
						list.addAll(getArgumentsFromDtreeList(Util.listToTermArray(s.get("BothClaim_D_Trees"))));
						
					}
					catch(PrologLoadException exception){
						exception.printStackTrace();
					}
					
					if(list != null) {
						DeltaExplanationDialog de = new DeltaExplanationDialog(list);
						de.setVisible(true);
					
						ArgumentsGraphDialog ad = new ArgumentsGraphDialog(getArgumentsForAndAgainst(claim));
						ad.setVisible(true);
						ad.setLocation(de.getX()+de.getWidth(), de.getY());
						
						de.getGraphPanel().setArgumentsPanel(ad.getGraphPanel());
						ad.getGraphPanel().setExplanationPanel(de.getGraphPanel());
						
					}
					
				}
				
			}
		});
		
		this.add(expandItem);
		
	}
	
	public void setEdge(AlternativesGraphEdge e){
		this.edge = e;
	}
	
	
	private ArrayList<Argument> getArgumentsFromDtreeList(Term [] list) throws PrologLoadException{
		
		ArrayList<Argument> arguments = new ArrayList<Argument>();
		
		for(Term t : list){
			
			Query q = new Query("dtree_node("+t.toString()+",null,_,Arg_Id,_)");
			
			if(q.hasNext()){
				Map<String, Term> s = q.next();
				
				Argument arg = new Argument(s.get("Arg_Id").toString());
				
				arguments.add(arg);
			}
			else{
				throw new PrologLoadException("No dtree_node has been found for Id "+t.toString());			
			}
			
		}
		
		return arguments;
	}
	
	
	private ArrayList<Argument> getArgumentsForAndAgainst(String claim) {
		ArrayList<Argument> toReturn = new ArrayList<Argument>();		
		
		Query q_for = new Query("argument(Arg_Id,_,"+claim+")");
		
		Map<String, Term>[] solutions = q_for.allSolutions();
		for(Map<String, Term> s : solutions) {
			toReturn.add(new Argument(s.get("Arg_Id").toString()));
		}
		
		Query q_against = new Query("complement("+claim+",CompClaim), argument(Arg_Id,_,CompClaim)");
		
		solutions = q_against.allSolutions();
		for(Map<String, Term> s : solutions) {
			toReturn.add(new Argument(s.get("Arg_Id").toString()));
		}
		
		return toReturn;	
	}
	
	
}

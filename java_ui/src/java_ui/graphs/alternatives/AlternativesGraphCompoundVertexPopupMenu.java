package java_ui.graphs.alternatives;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import edu.uci.ics.jung.visualization.VisualizationViewer;
import java_ui.arguments.Argument;
import java_ui.arguments.dialectical_trees.ArgumentsGraphDialog;
import java_ui.arguments.dialectical_trees.DeltaExplanationDialog;
import java_ui.graphs.alternatives.lattice.JungLatticeLayout;
import java_ui.prolog_loader.PrologLoadException;

public class AlternativesGraphCompoundVertexPopupMenu extends JPopupMenu{
	
	private AlternativesGraphVertex v;
	private JMenuItem expandItem;
	private JMenuItem addEdgesItem;
	private JMenuItem explainIncomparabilityItem;
	private Set<AlternativesGraphVertex> picked;
	
	public AlternativesGraphCompoundVertexPopupMenu(final AlternativesGraph graph, final VisualizationViewer<AlternativesGraphVertex, AlternativesGraphEdge> vv, final JungLatticeLayout<AlternativesGraphVertex, AlternativesGraphEdge> layout) {
		
		super();
		
		
		expandItem = new JMenuItem("Expand");
		expandItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				graph.expand((AlternativesGraphCompoundVertex)v);
				layout.update();
				vv.repaint();
			}
		});
		
		addEdgesItem = new JMenuItem("Add extra-edges");
		addEdgesItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				graph.addExtraEdgesBetween(picked);
				vv.repaint();
			}
		});
		
		explainIncomparabilityItem = new JMenuItem("Explain incomparability");
		explainIncomparabilityItem.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				explainIncomparability(picked);
				vv.repaint();
			}
			
		});
		
		this.add(expandItem);
		this.add(addEdgesItem);
		this.add(explainIncomparabilityItem);
		
	}


	public void setVertex(AlternativesGraphVertex v){
		this.v = v;	
		expandItem.setEnabled(v instanceof AlternativesGraphCompoundVertex);
		
		
		boolean flag = this.picked != null && this.picked.size() == 2;
		if(flag) {
			Iterator<AlternativesGraphVertex> it = this.picked.iterator();
			while(flag && it.hasNext()) {
				flag =  flag && it.next() instanceof AlternativesGraphSimpleVertex;
			}
		}
		
		Object[] pickedArray = flag ? this.picked.toArray() : null;
		addEdgesItem.setEnabled(flag && isPrefered((AlternativesGraphSimpleVertex)pickedArray[0],(AlternativesGraphSimpleVertex)pickedArray[1]));
		explainIncomparabilityItem.setEnabled(flag && incomparable((AlternativesGraphSimpleVertex)pickedArray[0],(AlternativesGraphSimpleVertex)pickedArray[1]));
	}
	
	
	private boolean incomparable(AlternativesGraphSimpleVertex v1, AlternativesGraphSimpleVertex v2) {
		Query q = new Query("incomparable("+v1.getId()+","+v2.getId()+")");
		
		boolean rtn = q.hasSolution();
		q.close();
		
		return rtn;
	}


	private boolean isPrefered(AlternativesGraphSimpleVertex v1, AlternativesGraphSimpleVertex v2) {
		Query q = new Query("explicitly_preferred("+v1.getId()+","+v2.getId()+"); explicitly_preferred("+v2.getId()+","+v1.getId()+")");
		
		boolean rtn = q.hasSolution();
		q.close();
		
		return rtn;

	}


	private void explainIncomparability(Set<AlternativesGraphVertex> picked) {
		Object[] aux = picked.toArray();
		String claim = "pref("+((AlternativesGraphVertex)aux[0]).getId()+","+((AlternativesGraphVertex)aux[1]).getId()+")";
		
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
	
	
	public void setPicked(Set<AlternativesGraphVertex> picked) {
		this.picked = picked;	
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

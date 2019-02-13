package java_ui.steps;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import java.awt.Font;
import javax.swing.JTextArea;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

import java_ui.graphs.alternatives.AlternativesGraphPanel;

import javax.swing.JScrollPane;
import javax.swing.JTable;

public class ResultsPanel extends JPanel {

	
	
	private JTextArea selectedAlternativesResult;
	private JTextArea relationResults;
	private JTable table;
	private AlternativesGraphPanel graphPanel;

	public ResultsPanel() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel mainResults = new JPanel();
		add(mainResults);
		GridBagLayout gbl_mainResults = new GridBagLayout();
		gbl_mainResults.columnWidths = new int[]{334, 0};
		gbl_mainResults.rowHeights = new int[]{14, 0, 155, 0, 38, 0, 161, 0};
		gbl_mainResults.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_mainResults.rowWeights = new double[]{0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		mainResults.setLayout(gbl_mainResults);
		
		JLabel titleLabel = new JLabel("Results");
		titleLabel.setFont(new Font("Tahoma", Font.PLAIN, 14));
		GridBagConstraints gbc_titleLabel = new GridBagConstraints();
		gbc_titleLabel.insets = new Insets(5, 5, 5, 0);
		gbc_titleLabel.anchor = GridBagConstraints.WEST;
		gbc_titleLabel.gridx = 0;
		gbc_titleLabel.gridy = 0;
		mainResults.add(titleLabel, gbc_titleLabel);
		
		JLabel assessmentsLabel = new JLabel("Assessments:");
		GridBagConstraints gbc_assessmentsLabel = new GridBagConstraints();
		gbc_assessmentsLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_assessmentsLabel.anchor = GridBagConstraints.NORTH;
		gbc_assessmentsLabel.insets = new Insets(0, 5, 5, 0);
		gbc_assessmentsLabel.gridx = 0;
		gbc_assessmentsLabel.gridy = 1;
		mainResults.add(assessmentsLabel, gbc_assessmentsLabel);
		
		JScrollPane scrollPane = new JScrollPane();
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.insets = new Insets(0, 0, 5, 0);
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 2;
		mainResults.add(scrollPane, gbc_scrollPane);
		
		table = new JTable();
		scrollPane.setViewportView(table);
		
		JLabel selectedAlternativesLabel = new JLabel("Selected Alternatives:");
		GridBagConstraints gbc_selectedAlternativesLabel = new GridBagConstraints();
		gbc_selectedAlternativesLabel.anchor = GridBagConstraints.WEST;
		gbc_selectedAlternativesLabel.insets = new Insets(0, 5, 5, 0);
		gbc_selectedAlternativesLabel.gridx = 0;
		gbc_selectedAlternativesLabel.gridy = 3;
		mainResults.add(selectedAlternativesLabel, gbc_selectedAlternativesLabel);
		
		selectedAlternativesResult = new JTextArea();
		selectedAlternativesResult.setLineWrap(true);
		selectedAlternativesResult.setEditable(false);
		GridBagConstraints gbc_selectedAlternativesResult = new GridBagConstraints();
		gbc_selectedAlternativesResult.insets = new Insets(0, 5, 5, 5);
		gbc_selectedAlternativesResult.fill = GridBagConstraints.BOTH;
		gbc_selectedAlternativesResult.gridx = 0;
		gbc_selectedAlternativesResult.gridy = 4;
		mainResults.add(selectedAlternativesResult, gbc_selectedAlternativesResult);
		
		JLabel alternativesRelationLabel = new JLabel("Alternatives Relation:");
		GridBagConstraints gbc_alternativesRelationLabel = new GridBagConstraints();
		gbc_alternativesRelationLabel.anchor = GridBagConstraints.WEST;
		gbc_alternativesRelationLabel.insets = new Insets(0, 5, 5, 0);
		gbc_alternativesRelationLabel.gridx = 0;
		gbc_alternativesRelationLabel.gridy = 5;
		mainResults.add(alternativesRelationLabel, gbc_alternativesRelationLabel);
		
		relationResults = new JTextArea();
		relationResults.setLineWrap(true);
		GridBagConstraints gbc_relationResults = new GridBagConstraints();
		gbc_relationResults.insets = new Insets(0, 5, 5, 5);
		gbc_relationResults.fill = GridBagConstraints.BOTH;
		gbc_relationResults.gridx = 0;
		gbc_relationResults.gridy = 6;
		//mainResults.add(relationResults, gbc_relationResults);
		
		graphPanel = new AlternativesGraphPanel();
		GridBagConstraints gbc_graphPanel = new GridBagConstraints();
		gbc_graphPanel.fill = GridBagConstraints.BOTH;
		gbc_graphPanel.insets = new Insets(0, 5, 5, 5);
		gbc_graphPanel.gridx = 0;
		gbc_graphPanel.gridy = 6;
		mainResults.add(graphPanel, gbc_graphPanel);
	}
	
	
	public void setSelectedAlternatives(String alternatives){
		selectedAlternativesResult.setText(alternatives);
	}
	
	public void setAlternativesRelation(String relation){
		relationResults.setText(relation);
	}
	
	public void loadAssessmentBase(){
		this.table.setModel(getAssessmentBase());
	}
	
	public void loadAlternativesGraph(){
		graphPanel.loadGraph();
	}
	
	
	private TableModel getAssessmentBase(){
		
		Vector<String> columnNames = getColumNames();
		Map<String,Integer> indexes = getColumnIndexes(columnNames);		
		DefaultTableModel table = new DefaultTableModel(columnNames,0);
			
		
		Query q = new Query("assessments(Alternative,Assessments)");
		
		Map<String,Term> solution;
		
		while(q.hasNext()){
			
			solution = q.next();
			
			Vector<String> data = new Vector<String>();
			data.setSize(columnNames.size());
			
			//Alternatives is always the first element.
			data.setElementAt(solution.get("Alternative").toString(),0);
			
			
			//Assessments is a list of pairs [Criterion, Value].
			Term [] assessments = Util.listToTermArray(solution.get("Assessments"));
			
			
			for(Term t : assessments){
				Term [] values = Util.listToTermArray(t);
				
				String criterion = values[0].toString();
				int index = indexes.get(criterion);
				
				if(Util.isList(values[1]))
					data.setElementAt(termArrayToString(Util.listToTermArray(values[1])), index);
				else{
					data.setElementAt(values[1].toString(), index);
				}
				
			}
			
			table.addRow(data);
			
		
		}
		
		return table;
	}
	
	
	private String termArrayToString(Term [] list){
		String toReturn = "[";
		
		int i;
		
		for(i=0; i < list.length-1; i++){
			toReturn += list[i]+", ";
		}
		
		toReturn += list[i];
		
		return toReturn + "]";
	}
	
	
	private Vector<String> getColumNames(){
		Vector<String> names = new Vector<String>();
		
		names.addElement("alternatives");
		
		Query q = new Query("criterion(C)");
		
		while(q.hasNext()){
			names.addElement(q.next().get("C").toString());
		}
		
		return names;
	}
	
	private Map<String,Integer> getColumnIndexes(Vector<String> columnNames){
		Map<String,Integer> columnIndexes = new HashMap<String,Integer>();
		
		for(int i = 0; i<columnNames.size(); i++){
			columnIndexes.put(columnNames.get(i), i);
		}
		
		return columnIndexes;
	}


	public void cleanAssessmentsBase() {
		this.table.setModel(new DefaultTableModel());
	}
	
	
	public void cleanGraph(){
		this.graphPanel.clearGraph();
	}

}

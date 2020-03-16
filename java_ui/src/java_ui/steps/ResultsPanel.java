package java_ui.steps;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import java.awt.BorderLayout;
import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Font;
import javax.swing.JTextArea;

import java_ui.arguments.dialectical_trees.ArgumentsGraphDialog;
import java_ui.graphs.alternatives.AlternativesGraphPanel;

import javax.swing.JTextField;
import javax.swing.JButton;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.ScrollPaneConstants;


public class ResultsPanel extends JPanel {
	private JTextArea relationResults;
	private JTextField selectedAlternativesText;
	private JTextField reasoningTimeText;
	private JTextField selectionTimeText;
	private JTextField argumentsCountText;
	private JButton graphButton;
	
	private ArgumentsGraphDialog graphDialog;
	private AlternativesGraphPanel graphPanel;
	private JScrollPane scrollPane;

	public ResultsPanel() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel mainResults = new JPanel();
		add(mainResults, BorderLayout.CENTER);
		GridBagLayout gbl_mainResults = new GridBagLayout();
		gbl_mainResults.columnWidths = new int[]{84, 65, 56, 136, 0};
		gbl_mainResults.rowHeights = new int[]{30, 382, 0, 38, 0, 28, 0, 0};
		gbl_mainResults.columnWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		gbl_mainResults.rowWeights = new double[]{0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		mainResults.setLayout(gbl_mainResults);
		
		JLabel titleLabel = new JLabel("Results");
		titleLabel.setFont(new Font("Tahoma", Font.PLAIN, 14));
		GridBagConstraints gbc_titleLabel = new GridBagConstraints();
		gbc_titleLabel.gridwidth = 4;
		gbc_titleLabel.insets = new Insets(5, 5, 5, 0);
		gbc_titleLabel.anchor = GridBagConstraints.WEST;
		gbc_titleLabel.gridx = 0;
		gbc_titleLabel.gridy = 0;
		mainResults.add(titleLabel, gbc_titleLabel);
		
		this.scrollPane = new JScrollPane();
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.gridwidth = 4;
		gbc_scrollPane.insets = new Insets(0, 0, 5, 5);
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 1;
		mainResults.add(scrollPane, gbc_scrollPane);
		
		this.graphPanel = new AlternativesGraphPanel();
		scrollPane.setViewportView(graphPanel);
		
		JLabel selectedAlternativesLabel = new JLabel("Selected Alternatives:");
		GridBagConstraints gbc_selectedAlternativesLabel = new GridBagConstraints();
		gbc_selectedAlternativesLabel.gridwidth = 4;
		gbc_selectedAlternativesLabel.anchor = GridBagConstraints.WEST;
		gbc_selectedAlternativesLabel.insets = new Insets(0, 5, 5, 0);
		gbc_selectedAlternativesLabel.gridx = 0;
		gbc_selectedAlternativesLabel.gridy = 2;
		mainResults.add(selectedAlternativesLabel, gbc_selectedAlternativesLabel);
		
		selectedAlternativesText = new JTextField();
		selectedAlternativesText.setEditable(false);
		GridBagConstraints gbc_selectedAlternativesText = new GridBagConstraints();
		gbc_selectedAlternativesText.fill = GridBagConstraints.HORIZONTAL;
		gbc_selectedAlternativesText.gridwidth = 3;
		gbc_selectedAlternativesText.insets = new Insets(0, 5, 5, 5);
		gbc_selectedAlternativesText.gridx = 0;
		gbc_selectedAlternativesText.gridy = 3;
		mainResults.add(selectedAlternativesText, gbc_selectedAlternativesText);
		selectedAlternativesText.setColumns(10);
		
		graphButton = new JButton("Arguments Graph");
		graphButton.setEnabled(false);
		graphButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				
				if(graphDialog == null){
					graphDialog = new ArgumentsGraphDialog();
				}
				graphDialog.setVisible(true);
				graphDialog.requestFocus();
				
			}
		});
		GridBagConstraints gbc_btnExplanationGraph = new GridBagConstraints();
		gbc_btnExplanationGraph.insets = new Insets(0, 0, 5, 0);
		gbc_btnExplanationGraph.gridx = 3;
		gbc_btnExplanationGraph.gridy = 3;
		mainResults.add(graphButton, gbc_btnExplanationGraph);
		
		JLabel lblStats = new JLabel("Stats");
		lblStats.setFont(new Font("Tahoma", Font.PLAIN, 14));
		GridBagConstraints gbc_lblStats = new GridBagConstraints();
		gbc_lblStats.gridwidth = 4;
		gbc_lblStats.anchor = GridBagConstraints.WEST;
		gbc_lblStats.insets = new Insets(0, 5, 5, 0);
		gbc_lblStats.gridx = 0;
		gbc_lblStats.gridy = 4;
		mainResults.add(lblStats, gbc_lblStats);
		
		JLabel alternativesRelationLabel = new JLabel("Reasonig Time:");
		GridBagConstraints gbc_alternativesRelationLabel = new GridBagConstraints();
		gbc_alternativesRelationLabel.anchor = GridBagConstraints.WEST;
		gbc_alternativesRelationLabel.insets = new Insets(0, 5, 5, 5);
		gbc_alternativesRelationLabel.gridx = 0;
		gbc_alternativesRelationLabel.gridy = 5;
		mainResults.add(alternativesRelationLabel, gbc_alternativesRelationLabel);
		
		reasoningTimeText = new JTextField();
		reasoningTimeText.setEditable(false);
		GridBagConstraints gbc_reasoningTimeText = new GridBagConstraints();
		gbc_reasoningTimeText.insets = new Insets(0, 5, 5, 5);
		gbc_reasoningTimeText.fill = GridBagConstraints.HORIZONTAL;
		gbc_reasoningTimeText.gridx = 1;
		gbc_reasoningTimeText.gridy = 5;
		mainResults.add(reasoningTimeText, gbc_reasoningTimeText);
		reasoningTimeText.setColumns(10);
		
		JLabel lblSelectionTime = new JLabel("Selection Time:");
		GridBagConstraints gbc_lblSelectionTime = new GridBagConstraints();
		gbc_lblSelectionTime.anchor = GridBagConstraints.WEST;
		gbc_lblSelectionTime.insets = new Insets(0, 5, 5, 5);
		gbc_lblSelectionTime.gridx = 2;
		gbc_lblSelectionTime.gridy = 5;
		mainResults.add(lblSelectionTime, gbc_lblSelectionTime);
		
		selectionTimeText = new JTextField();
		selectionTimeText.setEditable(false);
		GridBagConstraints gbc_selectionTimeText = new GridBagConstraints();
		gbc_selectionTimeText.fill = GridBagConstraints.HORIZONTAL;
		gbc_selectionTimeText.insets = new Insets(0, 0, 5, 0);
		gbc_selectionTimeText.gridx = 3;
		gbc_selectionTimeText.gridy = 5;
		mainResults.add(selectionTimeText, gbc_selectionTimeText);
		selectionTimeText.setColumns(10);
		
		JLabel lblArguments = new JLabel("Arguments:");
		GridBagConstraints gbc_lblArguments = new GridBagConstraints();
		gbc_lblArguments.anchor = GridBagConstraints.WEST;
		gbc_lblArguments.insets = new Insets(0, 5, 0, 5);
		gbc_lblArguments.gridx = 0;
		gbc_lblArguments.gridy = 6;
		mainResults.add(lblArguments, gbc_lblArguments);
		
		argumentsCountText = new JTextField();
		argumentsCountText.setEditable(false);
		GridBagConstraints gbc_argumentsAmountText = new GridBagConstraints();
		gbc_argumentsAmountText.insets = new Insets(0, 5, 0, 5);
		gbc_argumentsAmountText.fill = GridBagConstraints.HORIZONTAL;
		gbc_argumentsAmountText.gridx = 1;
		gbc_argumentsAmountText.gridy = 6;
		mainResults.add(argumentsCountText, gbc_argumentsAmountText);
		argumentsCountText.setColumns(10);
		
		relationResults = new JTextArea();
		relationResults.setLineWrap(true);
		GridBagConstraints gbc_relationResults = new GridBagConstraints();
		gbc_relationResults.insets = new Insets(0, 5, 5, 5);
		gbc_relationResults.fill = GridBagConstraints.BOTH;
		gbc_relationResults.gridx = 0;
		gbc_relationResults.gridy = 6;
	}
	
	
	public void setSelectedAlternatives(String alternatives){
		selectedAlternativesText.setText(alternatives);
	}
	
	public void setAlternativesRelation(String relation){
		relationResults.setText(relation);
	}
	
	public void loadAlternativesGraph() {
		this.graphPanel.loadGraph();
		this.scrollPane.setViewportView(this.graphPanel);
		
	}
	
	public void enableGraphButton(){
		graphButton.setEnabled(true);
	}
	
	public void disableGraphButton(){
		graphButton.setEnabled(false);
		
		cleanGraph();
	}
	
	public void cleanGraph(){
		if(graphDialog != null){
			graphDialog.setVisible(false);
			graphDialog.dispose();
			graphDialog = null;
		}
		if(this.graphPanel != null) {
			this.graphPanel.clearGraph();
		}
	}


	public void setArgumentsCount(String count) {
		argumentsCountText.setText(count);	
	}


	public void setReasoningTime(String time) {
		reasoningTimeText.setText(time);
		
	}


	public void setSelectionTime(String time) {
		selectionTimeText.setText(time);		
	}

}

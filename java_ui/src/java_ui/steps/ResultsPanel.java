package java_ui.steps;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Font;
import javax.swing.JTextArea;

import java_ui.arguments.dialectical_trees.ArgumentsGraphDialog;
import java_ui.graphs.alternatives.AlternativesGraphDialog;
import javax.swing.JTextField;
import javax.swing.JButton;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JSeparator;


public class ResultsPanel extends JPanel {
	private JTextArea relationResults;
	private JTextField selectedAlternativesText;
	private JTextField reasoningTimeText;
	private JTextField selectionTimeText;
	private JTextField argumentsCountText;
	private JButton alternativesGraphButton;
	private JButton argumentsGraphButton;
	
	private AlternativesGraphDialog alternativesGraphDialog;
	private ArgumentsGraphDialog argumentsGraphDialog;

	public ResultsPanel() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel mainResults = new JPanel();
		add(mainResults, BorderLayout.CENTER);
		GridBagLayout gbl_mainResults = new GridBagLayout();
		gbl_mainResults.columnWidths = new int[]{75, 130, 54, 130, 38, 0};
		gbl_mainResults.rowHeights = new int[]{30, 20, 20, 12, 0, 28, 0, 13, 0, 0, 0};
		gbl_mainResults.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		gbl_mainResults.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		mainResults.setLayout(gbl_mainResults);
		
		JLabel titleLabel = new JLabel("Results");
		titleLabel.setFont(new Font("Tahoma", Font.PLAIN, 14));
		GridBagConstraints gbc_titleLabel = new GridBagConstraints();
		gbc_titleLabel.gridwidth = 5;
		gbc_titleLabel.insets = new Insets(5, 5, 5, 5);
		gbc_titleLabel.anchor = GridBagConstraints.WEST;
		gbc_titleLabel.gridx = 0;
		gbc_titleLabel.gridy = 0;
		mainResults.add(titleLabel, gbc_titleLabel);
		
		JLabel selectedAlternativesLabel = new JLabel("Selected alternatives:");
		GridBagConstraints gbc_selectedAlternativesLabel = new GridBagConstraints();
		gbc_selectedAlternativesLabel.gridwidth = 5;
		gbc_selectedAlternativesLabel.anchor = GridBagConstraints.NORTHWEST;
		gbc_selectedAlternativesLabel.insets = new Insets(0, 5, 5, 5);
		gbc_selectedAlternativesLabel.gridx = 0;
		gbc_selectedAlternativesLabel.gridy = 1;
		mainResults.add(selectedAlternativesLabel, gbc_selectedAlternativesLabel);
		
		selectedAlternativesText = new JTextField();
		selectedAlternativesText.setEditable(false);
		GridBagConstraints gbc_selectedAlternativesText = new GridBagConstraints();
		gbc_selectedAlternativesText.fill = GridBagConstraints.HORIZONTAL;
		gbc_selectedAlternativesText.gridwidth = 4;
		gbc_selectedAlternativesText.anchor = GridBagConstraints.NORTH;
		gbc_selectedAlternativesText.insets = new Insets(0, 5, 5, 5);
		gbc_selectedAlternativesText.gridx = 0;
		gbc_selectedAlternativesText.gridy = 2;
		mainResults.add(selectedAlternativesText, gbc_selectedAlternativesText);
		selectedAlternativesText.setColumns(10);
		
		JSeparator separator = new JSeparator();
		GridBagConstraints gbc_separator = new GridBagConstraints();
		gbc_separator.fill = GridBagConstraints.BOTH;
		gbc_separator.gridwidth = 5;
		gbc_separator.insets = new Insets(0, 0, 5, 5);
		gbc_separator.gridx = 0;
		gbc_separator.gridy = 3;
		mainResults.add(separator, gbc_separator);
		
		JLabel lblStats = new JLabel("Stats");
		lblStats.setFont(new Font("Tahoma", Font.PLAIN, 14));
		GridBagConstraints gbc_lblStats = new GridBagConstraints();
		gbc_lblStats.gridwidth = 5;
		gbc_lblStats.anchor = GridBagConstraints.WEST;
		gbc_lblStats.insets = new Insets(0, 5, 5, 5);
		gbc_lblStats.gridx = 0;
		gbc_lblStats.gridy = 4;
		mainResults.add(lblStats, gbc_lblStats);
		
		JLabel alternativesRelationLabel = new JLabel("Reasonig time:");
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
		
		JLabel lblSelectionTime = new JLabel("Selection time:");
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
		gbc_selectionTimeText.insets = new Insets(0, 0, 5, 5);
		gbc_selectionTimeText.gridx = 3;
		gbc_selectionTimeText.gridy = 5;
		mainResults.add(selectionTimeText, gbc_selectionTimeText);
		selectionTimeText.setColumns(10);
		
		JLabel lblArguments = new JLabel("Arguments:");
		GridBagConstraints gbc_lblArguments = new GridBagConstraints();
		gbc_lblArguments.anchor = GridBagConstraints.WEST;
		gbc_lblArguments.insets = new Insets(0, 5, 5, 5);
		gbc_lblArguments.gridx = 0;
		gbc_lblArguments.gridy = 6;
		mainResults.add(lblArguments, gbc_lblArguments);
		
		argumentsCountText = new JTextField();
		argumentsCountText.setEditable(false);
		GridBagConstraints gbc_argumentsAmountText = new GridBagConstraints();
		gbc_argumentsAmountText.insets = new Insets(0, 5, 5, 5);
		gbc_argumentsAmountText.fill = GridBagConstraints.HORIZONTAL;
		gbc_argumentsAmountText.gridx = 1;
		gbc_argumentsAmountText.gridy = 6;
		mainResults.add(argumentsCountText, gbc_argumentsAmountText);
		argumentsCountText.setColumns(10);
		
		JSeparator separator_1 = new JSeparator();
		GridBagConstraints gbc_separator_1 = new GridBagConstraints();
		gbc_separator_1.gridwidth = 5;
		gbc_separator_1.fill = GridBagConstraints.BOTH;
		gbc_separator_1.insets = new Insets(0, 0, 5, 5);
		gbc_separator_1.gridx = 0;
		gbc_separator_1.gridy = 7;
		mainResults.add(separator_1, gbc_separator_1);
		
		JPanel panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.insets = new Insets(0, 0, 5, 5);
		gbc_panel.gridwidth = 5;
		gbc_panel.fill = GridBagConstraints.HORIZONTAL;
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 8;
		mainResults.add(panel, gbc_panel);
		GridBagLayout gbl_panel = new GridBagLayout();
		gbl_panel.columnWidths = new int[]{0, 0, 0};
		gbl_panel.rowHeights = new int[]{0, 0};
		gbl_panel.columnWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);
		
		alternativesGraphButton = new JButton("Explanation graph");
		alternativesGraphButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				if(alternativesGraphDialog == null){
					alternativesGraphDialog = new AlternativesGraphDialog();
				}
				alternativesGraphDialog.setVisible(true);
				alternativesGraphDialog.requestFocus();
				
			}
		});
		GridBagConstraints gbc_btnAlternatives = new GridBagConstraints();
		gbc_btnAlternatives.insets = new Insets(0, 0, 0, 5);
		gbc_btnAlternatives.gridx = 0;
		gbc_btnAlternatives.gridy = 0;
		panel.add(alternativesGraphButton, gbc_btnAlternatives);
		alternativesGraphButton.setEnabled(false);
		
		argumentsGraphButton = new JButton("Argument graphs");
		GridBagConstraints gbc_graphButton = new GridBagConstraints();
		gbc_graphButton.gridx = 1;
		gbc_graphButton.gridy = 0;
		panel.add(argumentsGraphButton, gbc_graphButton);
		argumentsGraphButton.setEnabled(false);
		argumentsGraphButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				
				if(argumentsGraphDialog == null){
					argumentsGraphDialog = new ArgumentsGraphDialog();
				}
				argumentsGraphDialog.setVisible(true);
				argumentsGraphDialog.requestFocus();
				
			}
		});
		
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
		if(alternativesGraphDialog == null){
			alternativesGraphDialog = new AlternativesGraphDialog();
		}
		alternativesGraphDialog.setVisible(true);
		alternativesGraphDialog.requestFocus();	
	}
	
	public void enableGraphButtons(){
		alternativesGraphButton.setEnabled(true);
		argumentsGraphButton.setEnabled(true);
	}
	
	public void disableGraphButtons() {
		alternativesGraphButton.setEnabled(false);
		argumentsGraphButton.setEnabled(false);
		
		cleanGraphs();
	}
	
	public void cleanGraphs(){
		if(alternativesGraphDialog != null){
			alternativesGraphDialog.setVisible(false);
			alternativesGraphDialog.dispose();
			alternativesGraphDialog = null;
		}
		if(argumentsGraphDialog != null){
			argumentsGraphDialog.setVisible(false);
			argumentsGraphDialog.dispose();
			argumentsGraphDialog = null;
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

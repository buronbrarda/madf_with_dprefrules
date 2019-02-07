package java_ui.steps;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Font;
import javax.swing.JTextArea;

public class ResultsPanel extends JPanel {

	
	
	private JTextArea selectedAlternativesResult;
	private JTextArea relationResults;

	public ResultsPanel() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel mainResults = new JPanel();
		add(mainResults);
		GridBagLayout gbl_mainResults = new GridBagLayout();
		gbl_mainResults.columnWidths = new int[]{334, 0};
		gbl_mainResults.rowHeights = new int[]{14, 0, 110, 0, 118, 0};
		gbl_mainResults.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_mainResults.rowWeights = new double[]{0.0, 0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE};
		mainResults.setLayout(gbl_mainResults);
		
		JLabel titleLabel = new JLabel("Results");
		titleLabel.setFont(new Font("Tahoma", Font.PLAIN, 14));
		GridBagConstraints gbc_titleLabel = new GridBagConstraints();
		gbc_titleLabel.insets = new Insets(5, 5, 5, 0);
		gbc_titleLabel.anchor = GridBagConstraints.WEST;
		gbc_titleLabel.gridx = 0;
		gbc_titleLabel.gridy = 0;
		mainResults.add(titleLabel, gbc_titleLabel);
		
		JLabel selectedAlternativesLabel = new JLabel("Selected Alternatives:");
		GridBagConstraints gbc_selectedAlternativesLabel = new GridBagConstraints();
		gbc_selectedAlternativesLabel.anchor = GridBagConstraints.WEST;
		gbc_selectedAlternativesLabel.insets = new Insets(0, 5, 5, 0);
		gbc_selectedAlternativesLabel.gridx = 0;
		gbc_selectedAlternativesLabel.gridy = 1;
		mainResults.add(selectedAlternativesLabel, gbc_selectedAlternativesLabel);
		
		selectedAlternativesResult = new JTextArea();
		selectedAlternativesResult.setEditable(false);
		GridBagConstraints gbc_selectedAlternativesResult = new GridBagConstraints();
		gbc_selectedAlternativesResult.insets = new Insets(0, 5, 5, 5);
		gbc_selectedAlternativesResult.fill = GridBagConstraints.BOTH;
		gbc_selectedAlternativesResult.gridx = 0;
		gbc_selectedAlternativesResult.gridy = 2;
		mainResults.add(selectedAlternativesResult, gbc_selectedAlternativesResult);
		
		JLabel alternativesRelationLabel = new JLabel("Alternatives Relation:");
		GridBagConstraints gbc_alternativesRelationLabel = new GridBagConstraints();
		gbc_alternativesRelationLabel.anchor = GridBagConstraints.WEST;
		gbc_alternativesRelationLabel.insets = new Insets(0, 5, 5, 0);
		gbc_alternativesRelationLabel.gridx = 0;
		gbc_alternativesRelationLabel.gridy = 3;
		mainResults.add(alternativesRelationLabel, gbc_alternativesRelationLabel);
		
		relationResults = new JTextArea();
		GridBagConstraints gbc_relationResults = new GridBagConstraints();
		gbc_relationResults.insets = new Insets(0, 5, 5, 5);
		gbc_relationResults.fill = GridBagConstraints.BOTH;
		gbc_relationResults.gridx = 0;
		gbc_relationResults.gridy = 4;
		mainResults.add(relationResults, gbc_relationResults);

	}
	
	
	public void setSelectedAlternatives(String alternatives){
		selectedAlternativesResult.setText(alternatives);
	}
	
	public void setAlternativesRelation(String relation){
		relationResults.setText(relation);
	}

}

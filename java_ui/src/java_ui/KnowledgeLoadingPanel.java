package java_ui;

import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JLabel;
import javax.swing.BoxLayout;
import java.awt.Component;
import javax.swing.SwingConstants;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.GridLayout;
import javax.swing.JButton;
import java.awt.FlowLayout;
import java.awt.BorderLayout;

public class KnowledgeLoadingPanel extends JPanel {
	private JTextField filePath;

	/**
	 * Create the panel.
	 */
	public KnowledgeLoadingPanel() {
		setLayout(new GridLayout(0, 1, 0, 0));
		
		JPanel instructionPanel = new JPanel();
		FlowLayout fl_instructionPanel = (FlowLayout) instructionPanel.getLayout();
		fl_instructionPanel.setAlignment(FlowLayout.LEFT);
		add(instructionPanel);
		
		JLabel intructionLabel = new JLabel("2. Load the knowledge base file.  The file must be on CSV format.");
		instructionPanel.add(intructionLabel);
		
		JPanel fileUploadPanel = new JPanel();
		add(fileUploadPanel);
		GridBagLayout gbl_fileUploadPanel = new GridBagLayout();
		gbl_fileUploadPanel.columnWidths = new int[]{0, 311, 75, 0, 0, 0, 0};
		gbl_fileUploadPanel.rowHeights = new int[]{38, 0};
		gbl_fileUploadPanel.columnWeights = new double[]{0.0, 1.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_fileUploadPanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		fileUploadPanel.setLayout(gbl_fileUploadPanel);
		
		JLabel fileLabel = new JLabel("File:");
		GridBagConstraints gbc_fileLabel = new GridBagConstraints();
		gbc_fileLabel.anchor = GridBagConstraints.WEST;
		gbc_fileLabel.insets = new Insets(0, 5, 0, 5);
		gbc_fileLabel.gridx = 0;
		gbc_fileLabel.gridy = 0;
		fileUploadPanel.add(fileLabel, gbc_fileLabel);
		
		filePath = new JTextField();
		GridBagConstraints gbc_filePath = new GridBagConstraints();
		gbc_filePath.gridwidth = 3;
		gbc_filePath.fill = GridBagConstraints.HORIZONTAL;
		gbc_filePath.insets = new Insets(0, 0, 0, 5);
		gbc_filePath.gridx = 1;
		gbc_filePath.gridy = 0;
		fileUploadPanel.add(filePath, gbc_filePath);
		filePath.setAlignmentX(Component.LEFT_ALIGNMENT);
		filePath.setColumns(50);
		filePath.setEditable(false);
		
		JButton fileLoadButton = new JButton("Load File");
		GridBagConstraints gbc_fileLoadButton = new GridBagConstraints();
		gbc_fileLoadButton.gridwidth = 2;
		gbc_fileLoadButton.insets = new Insets(0, 0, 0, 5);
		gbc_fileLoadButton.gridx = 4;
		gbc_fileLoadButton.gridy = 0;
		fileUploadPanel.add(fileLoadButton, gbc_fileLoadButton);

	}

}

package java_ui;

import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JLabel;
import javax.swing.BoxLayout;
import java.awt.Component;
import javax.swing.SwingConstants;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.filechooser.FileSystemView;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.GridLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;

import java.awt.FlowLayout;
import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.io.File;
import java.awt.event.ActionEvent;

public class CriteriaLoadingPanel extends JPanel {
	private JTextField filePathText;
	private File selectedFile;

	/**
	 * Create the panel.
	 */
	public CriteriaLoadingPanel() {
		setLayout(new BorderLayout(0, 0));
		
		JPanel panel = new JPanel();
		add(panel);
		GridBagLayout gbl_panel = new GridBagLayout();
		gbl_panel.columnWidths = new int[]{23, 158, 123, 0};
		gbl_panel.rowHeights = new int[]{23, 0, 0};
		gbl_panel.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);
		
		JLabel intructionLabel = new JLabel("1. Define the set of criteria and their assessments values.");
		GridBagConstraints gbc_intructionLabel = new GridBagConstraints();
		gbc_intructionLabel.gridwidth = 2;
		gbc_intructionLabel.anchor = GridBagConstraints.WEST;
		gbc_intructionLabel.insets = new Insets(5, 5, 5, 5);
		gbc_intructionLabel.gridx = 0;
		gbc_intructionLabel.gridy = 0;
		panel.add(intructionLabel, gbc_intructionLabel);
		
		JButton defineButton = new JButton("Define\r\n");
		GridBagConstraints gbc_defineButton = new GridBagConstraints();
		gbc_defineButton.insets = new Insets(5, 0, 5, 5);
		gbc_defineButton.gridx = 2;
		gbc_defineButton.gridy = 0;
		panel.add(defineButton, gbc_defineButton);
		
		JLabel filePathLabel = new JLabel("File:");
		GridBagConstraints gbc_filePathLabel = new GridBagConstraints();
		gbc_filePathLabel.anchor = GridBagConstraints.WEST;
		gbc_filePathLabel.insets = new Insets(0, 5, 5, 5);
		gbc_filePathLabel.gridx = 0;
		gbc_filePathLabel.gridy = 1;
		panel.add(filePathLabel, gbc_filePathLabel);
		
		filePathText = new JTextField();
		filePathText.setEditable(false);
		GridBagConstraints gbc_filePathText = new GridBagConstraints();
		gbc_filePathText.fill = GridBagConstraints.HORIZONTAL;
		gbc_filePathText.insets = new Insets(0, 0, 5, 5);
		gbc_filePathText.gridx = 1;
		gbc_filePathText.gridy = 1;
		panel.add(filePathText, gbc_filePathText);
		filePathText.setColumns(10);
		
		JButton loadFileButton = new JButton("Load File");
		loadFileButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				File f = loadCSVFile();
				
				if(f != null){
					filePathText.setText(f.getAbsolutePath());
					selectedFile = f;
				}
			}
		});
		GridBagConstraints gbc_loadFileButton = new GridBagConstraints();
		gbc_loadFileButton.insets = new Insets(0, 0, 5, 5);
		gbc_loadFileButton.gridx = 2;
		gbc_loadFileButton.gridy = 1;
		panel.add(loadFileButton, gbc_loadFileButton);
		defineButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				new CriteriaEditorDialog();
			}
		});

	}
	
	
	private File loadCSVFile(){
		
		JFileChooser fc = new JFileChooser("./");
		fc.setDialogTitle("Select the file to load");
		
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fc.setFileFilter(new FileNameExtensionFilter("CSV File", "csv"));
		fc.setMultiSelectionEnabled(false);
		
		File toReturn = null;
		
		if(fc.showDialog(null, "Load") == JFileChooser.APPROVE_OPTION){
			toReturn = fc.getSelectedFile();
		}
		
		
		return toReturn;
	}

}

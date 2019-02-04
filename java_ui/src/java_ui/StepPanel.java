package java_ui;

import javax.swing.JPanel;
import java.awt.GridBagLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import java.awt.GridBagConstraints;
import javax.swing.JButton;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.awt.event.ActionEvent;

public class StepPanel extends JPanel{
	
	public StepPanel(String instruction, TableEditorPanel tep, PrologLoader loader) {
		super();
		
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{0, 0, 0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0, 0};
		gridBagLayout.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JLabel instructionLabel = new JLabel(instruction);
		GridBagConstraints gbc_instructionLabel = new GridBagConstraints();
		gbc_instructionLabel.anchor = GridBagConstraints.WEST;
		gbc_instructionLabel.gridwidth = 2;
		gbc_instructionLabel.insets = new Insets(5, 5, 5, 5);
		gbc_instructionLabel.gridx = 0;
		gbc_instructionLabel.gridy = 0;
		add(instructionLabel, gbc_instructionLabel);
		
		JButton stepButton = new JButton("Define");
		stepButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					TableEditorDialog dialog = new TableEditorDialog(tep);
					loader.loadData(dialog.getResponse());
				} 
				catch (PrologLoadException e1) {
					e1.printStackTrace();
					JOptionPane.showMessageDialog(null, e1.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
				}
				catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		});
		GridBagConstraints gbc_stepButton = new GridBagConstraints();
		gbc_stepButton.anchor = GridBagConstraints.EAST;
		gbc_stepButton.insets = new Insets(5, 0, 5, 5);
		gbc_stepButton.gridx = 2;
		gbc_stepButton.gridy = 0;
		add(stepButton, gbc_stepButton);
		
		JLabel statusLabel = new JLabel("Status:");
		GridBagConstraints gbc_statusLabel = new GridBagConstraints();
		gbc_statusLabel.anchor = GridBagConstraints.WEST;
		gbc_statusLabel.insets = new Insets(0, 5, 5, 5);
		gbc_statusLabel.gridx = 0;
		gbc_statusLabel.gridy = 1;
		add(statusLabel, gbc_statusLabel);
		
		JLabel statusResultLabel = new JLabel("---");
		GridBagConstraints gbc_statusResultLabel = new GridBagConstraints();
		gbc_statusResultLabel.insets = new Insets(0, 0, 5, 5);
		gbc_statusResultLabel.anchor = GridBagConstraints.WEST;
		gbc_statusResultLabel.gridx = 1;
		gbc_statusResultLabel.gridy = 1;
		add(statusResultLabel, gbc_statusResultLabel);
		
	}
}

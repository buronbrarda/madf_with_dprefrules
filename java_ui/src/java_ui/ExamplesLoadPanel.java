package java_ui;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import javax.swing.JSeparator;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.awt.event.ActionEvent;
import java.awt.BorderLayout;

public class ExamplesLoadPanel extends JPanel {

	/**
	 * Create the panel.
	 */
	public ExamplesLoadPanel(AllStepsPanel stepsPanel) {
		setLayout(new BorderLayout(0, 0));
		
		JPanel container = new JPanel();
		GridBagLayout gbc_container = new GridBagLayout();
		gbc_container.columnWidths = new int[]{0, 66, 118, 95, 0};
		gbc_container.rowHeights = new int[]{37, 39, 0, 0};
		gbc_container.columnWeights = new double[]{};
		gbc_container.rowWeights = new double[]{};
		add(container, BorderLayout.CENTER);
		GridBagLayout gbl_container = new GridBagLayout();
		gbl_container.columnWidths = new int[]{70, 79, 84, 0};
		gbl_container.rowHeights = new int[]{25, 25, 0};
		gbl_container.columnWeights = new double[]{1.0, 1.0, 1.0, Double.MIN_VALUE};
		gbl_container.rowWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		container.setLayout(gbl_container);
		
		JLabel instructionLabel = new JLabel("Click on the buttons to load any example.");
		GridBagConstraints gbc_instructionLabel = new GridBagConstraints();
		gbc_instructionLabel.insets = new Insets(5, 5, 0, 5);
		gbc_instructionLabel.anchor = GridBagConstraints.WEST;
		gbc_instructionLabel.gridwidth = 3;
		gbc_instructionLabel.gridx = 0;
		container.add(instructionLabel, gbc_instructionLabel);
		
		JButton timButton = new JButton("Tim Example");
		GridBagConstraints gbc_timButton = new GridBagConstraints();
		gbc_timButton.anchor = GridBagConstraints.NORTH;
		gbc_timButton.insets = new Insets(5, 5, 5, 5);
		gbc_timButton.gridx = 0;
		gbc_timButton.gridy = 1;
		container.add(timButton, gbc_timButton);
		timButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					stepsPanel.loadExample(1, "Tim");
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		});
		
		JButton augustButton = new JButton("August Example");
		GridBagConstraints gbc_augustButton = new GridBagConstraints();
		gbc_augustButton.anchor = GridBagConstraints.NORTH;
		gbc_augustButton.insets = new Insets(5, 0, 5, 5);
		gbc_augustButton.gridx = 1;
		gbc_augustButton.gridy = 1;
		container.add(augustButton, gbc_augustButton);
		augustButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					stepsPanel.loadExample(1, "August");
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		});
		
		JButton kateButton = new JButton("Kate Example");
		GridBagConstraints gbc_kateButton = new GridBagConstraints();
		gbc_kateButton.insets = new Insets(5, 0, 5, 5);
		gbc_kateButton.anchor = GridBagConstraints.NORTH;
		gbc_kateButton.gridx = 2;
		gbc_kateButton.gridy = 1;
		container.add(kateButton, gbc_kateButton);
		kateButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					stepsPanel.loadExample(1, "Kate");
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		});
		
		JSeparator separator = new JSeparator();
		add(separator, BorderLayout.SOUTH);

	}

}

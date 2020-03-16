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
		gbl_container.columnWidths = new int[]{45, 110, 0};
		gbl_container.rowHeights = new int[]{70, 0};
		gbl_container.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		gbl_container.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		container.setLayout(gbl_container);
		
		JButton loadButton = new JButton("Load Example");
		loadButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					stepsPanel.loadExample();
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		});
		
		JLabel instructionLabel = new JLabel("Click on the button to load an example.");
		GridBagConstraints gbc_instructionLabel = new GridBagConstraints();
		gbc_instructionLabel.gridy = 0;
		gbc_instructionLabel.insets = new Insets(5, 5, 0, 5);
		gbc_instructionLabel.anchor = GridBagConstraints.WEST;
		gbc_instructionLabel.gridx = 0;
		container.add(instructionLabel, gbc_instructionLabel);
		GridBagConstraints gbc_loadButton = new GridBagConstraints();
		gbc_loadButton.gridx = 1;
		gbc_loadButton.gridy = 0;
		container.add(loadButton, gbc_loadButton);
		
		JSeparator separator = new JSeparator();
		add(separator, BorderLayout.SOUTH);
	
	}

}

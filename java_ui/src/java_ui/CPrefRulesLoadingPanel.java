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
import javax.swing.JDialog;

import java.awt.FlowLayout;
import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class CPrefRulesLoadingPanel extends JPanel {

	/**
	 * Create the panel.
	 */
	public CPrefRulesLoadingPanel() {
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{210, 0, 0};
		gridBagLayout.rowHeights = new int[]{85, 0};
		gridBagLayout.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JLabel intructionLabel = new JLabel("2. Define the set of CPref-Rules.");
		GridBagConstraints gbc_intructionLabel = new GridBagConstraints();
		gbc_intructionLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_intructionLabel.insets = new Insets(0, 5, 0, 5);
		gbc_intructionLabel.gridx = 0;
		gbc_intructionLabel.gridy = 0;
		add(intructionLabel, gbc_intructionLabel);
		
		JButton btnNewButton = new JButton("Define CPref-Rules");
		btnNewButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				new CPrefRulesEditorDialog();
			}
		});
		GridBagConstraints gbc_btnNewButton = new GridBagConstraints();
		gbc_btnNewButton.insets = new Insets(0, 0, 0, 5);
		gbc_btnNewButton.gridx = 1;
		gbc_btnNewButton.gridy = 0;
		add(btnNewButton, gbc_btnNewButton);

	}

}

package java_ui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JSeparator;
import javax.swing.BoxLayout;
import javax.swing.SwingConstants;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

public class CriteriaEditorDialog extends JDialog {

	/**
	 * Create the dialog.
	 */
	public CriteriaEditorDialog() {
		this.setModalityType(ModalityType.APPLICATION_MODAL);
		
		setBounds(100, 100, 450, 300);
		getContentPane().setLayout(new BorderLayout());
		CriteriaEditorPanel criteriaEditorPanel = new CriteriaEditorPanel();
		getContentPane().add(criteriaEditorPanel, BorderLayout.CENTER);
		
		JSeparator separator = new JSeparator();
		GridBagConstraints gbc_separator = new GridBagConstraints();
		gbc_separator.gridx = 0;
		gbc_separator.gridy = 2;
		criteriaEditorPanel.add(separator, gbc_separator);
		
		{
			JPanel buttonPane = new JPanel();
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				GridBagLayout gbl_buttonPane = new GridBagLayout();
				gbl_buttonPane.columnWidths = new int[]{193, 47, 0};
				gbl_buttonPane.rowHeights = new int[]{23, 0};
				gbl_buttonPane.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
				gbl_buttonPane.rowWeights = new double[]{0.0, Double.MIN_VALUE};
				buttonPane.setLayout(gbl_buttonPane);
			}
			JButton okButton = new JButton("OK");
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
					setVisible(false);
					dispose();
				}
			});
			okButton.setActionCommand("OK");
			GridBagConstraints gbc_okButton = new GridBagConstraints();
			gbc_okButton.insets = new Insets(5, 0, 5, 5);
			gbc_okButton.anchor = GridBagConstraints.NORTHWEST;
			gbc_okButton.gridx = 1;
			gbc_okButton.gridy = 0;
			buttonPane.add(okButton, gbc_okButton);
			getRootPane().setDefaultButton(okButton);
		}
		
		
		this.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		this.setVisible(true);
		
	}

}

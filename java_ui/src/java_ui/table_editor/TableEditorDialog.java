package java_ui.table_editor;

import java.awt.BorderLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.awt.event.ActionEvent;
import javax.swing.table.TableModel;

import java_ui.table_editor.panel.TableEditorPanel;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;


public class TableEditorDialog extends JDialog {
	
	private TableModel response;
	
	
	/**
	 * Create the dialog.
	 * @throws IOException 
	 */
	public TableEditorDialog(final TableEditorPanel tep) throws IOException {
		this.setModalityType(ModalityType.APPLICATION_MODAL);
		
		setBounds(100, 100, 450, 300);
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(tep, BorderLayout.CENTER);
		
		
		JPanel buttonPane = new JPanel();
		getContentPane().add(buttonPane, BorderLayout.SOUTH);
		
		GridBagLayout gbl_buttonPane = new GridBagLayout();
		gbl_buttonPane.columnWidths = new int[]{193, 47, 0, 0};
		gbl_buttonPane.rowHeights = new int[]{23, 0};
		gbl_buttonPane.columnWeights = new double[]{1.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_buttonPane.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		buttonPane.setLayout(gbl_buttonPane);
		
		
		JButton okButton = new JButton("OK");
		okButton.setActionCommand("OK");
		GridBagConstraints gbc_okButton = new GridBagConstraints();
		gbc_okButton.insets = new Insets(5, 0, 5, 5);
		gbc_okButton.anchor = GridBagConstraints.EAST;
		gbc_okButton.gridx = 1;
		gbc_okButton.gridy = 0;
		buttonPane.add(okButton, gbc_okButton);
		
		
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				response = tep.getTableModel();
				setVisible(false);
				dispose();
			}
		});
		
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setActionCommand("Cancel");
		GridBagConstraints gbc_cancelButton = new GridBagConstraints();
		gbc_cancelButton.anchor = GridBagConstraints.EAST;
		gbc_cancelButton.insets = new Insets(5, 0, 5, 5);
		gbc_cancelButton.gridx = 2;
		gbc_cancelButton.gridy = 0;
		buttonPane.add(cancelButton, gbc_cancelButton);
		
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				response = null;
				setVisible(false);
				dispose();
			}
		});
		
		getRootPane().setDefaultButton(cancelButton);
		
		
		
		this.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		this.setVisible(true);
		
	}
	
	
	public TableModel getResponse() {
		return response;
	}

}

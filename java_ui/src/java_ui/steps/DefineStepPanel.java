package java_ui.steps;

import javax.swing.table.TableModel;

import java_ui.prolog_loader.PrologLoadException;
import java_ui.prolog_loader.PrologLoader;
import java_ui.table_editor.TableEditorDialog;
import java_ui.table_editor.panel.TableEditorPanel;
import java_ui.table_editor.panel.TableViewer;

import java.awt.GridBagLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import java.awt.GridBagConstraints;
import javax.swing.JButton;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.awt.event.ActionEvent;

public class DefineStepPanel extends StepPanel{
	
	private JButton stepButton;
	private JLabel statusLabel;
	private JLabel statusResultLabel;
	
	private TableEditorPanel tep;
	private PrologLoader loader;
	private JButton viewButton;

	public DefineStepPanel(String instruction, TableEditorPanel tep, PrologLoader loader) {
		this.tep = tep;
		this.loader = loader;
		
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{47, 0, 0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0, 0};
		gridBagLayout.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, 1.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JLabel instructionLabel = new JLabel(instruction);
		GridBagConstraints gbc_instructionLabel = new GridBagConstraints();
		gbc_instructionLabel.anchor = GridBagConstraints.NORTH;
		gbc_instructionLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_instructionLabel.gridwidth = 2;
		gbc_instructionLabel.insets = new Insets(5, 5, 5, 5);
		gbc_instructionLabel.gridx = 0;
		gbc_instructionLabel.gridy = 0;
		add(instructionLabel, gbc_instructionLabel);
		
		this.stepButton = new JButton("Edit");
		stepButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					TableEditorDialog dialog = new TableEditorDialog(tep);
					
					TableModel response = dialog.getResponse();
					
					if(response != null){
						loader.loadData(response);
						
						if(loader.getStatus() == PrologLoader.StatusCode.Ok){
							statusResultLabel.setText("OK");
							getFollowingStep().enableStep();
						}
						else{
							statusResultLabel.setText("ERROR");
							getFollowingStep().disableStep();
						}
					}
				} 
				catch (PrologLoadException e1) {
					getFollowingStep().disableStep();
					statusResultLabel.setText("ERROR");
					e1.printStackTrace();
					JOptionPane.showMessageDialog(null, e1.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
				}
				catch (IOException e1) {
					getFollowingStep().disableStep();
					statusResultLabel.setText("ERROR");
					e1.printStackTrace();
				}
			}
		});
		GridBagConstraints gbc_stepButton = new GridBagConstraints();
		gbc_stepButton.anchor = GridBagConstraints.NORTH;
		gbc_stepButton.fill = GridBagConstraints.HORIZONTAL;
		gbc_stepButton.insets = new Insets(5, 5, 5, 5);
		gbc_stepButton.gridx = 2;
		gbc_stepButton.gridy = 0;
		add(stepButton, gbc_stepButton);
		
		this.statusLabel = new JLabel("Status:");
		GridBagConstraints gbc_statusLabel = new GridBagConstraints();
		gbc_statusLabel.anchor = GridBagConstraints.NORTH;
		gbc_statusLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_statusLabel.insets = new Insets(0, 5, 5, 5);
		gbc_statusLabel.gridx = 0;
		gbc_statusLabel.gridy = 1;
		add(statusLabel, gbc_statusLabel);
		
		this.statusResultLabel = new JLabel("---");
		GridBagConstraints gbc_statusResultLabel = new GridBagConstraints();
		gbc_statusResultLabel.anchor = GridBagConstraints.NORTH;
		gbc_statusResultLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_statusResultLabel.insets = new Insets(0, 5, 5, 5);
		gbc_statusResultLabel.gridx = 1;
		gbc_statusResultLabel.gridy = 1;
		add(statusResultLabel, gbc_statusResultLabel);
		
		viewButton = new JButton("View");
		viewButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {			
				new TableViewer(tep.getTableModel());
			}
		});
		GridBagConstraints gbc_viewButton = new GridBagConstraints();
		gbc_viewButton.insets = new Insets(0, 5, 5, 5);
		gbc_viewButton.fill = GridBagConstraints.HORIZONTAL;
		gbc_viewButton.anchor = GridBagConstraints.NORTH;
		gbc_viewButton.gridx = 2;
		gbc_viewButton.gridy = 1;
		add(viewButton, gbc_viewButton);
		
	}

	@Override
	public void enableStep() {
		this.stepButton.setEnabled(true);
		this.viewButton.setEnabled(true);
	}

	@Override
	public void disableStepAction() {
		this.stepButton.setEnabled(false);
		this.viewButton.setEnabled(false);
	}
	
	
	public void setTableModel(TableModel tm){
		this.tep.setTableModel(tm);
		try {
			this.loader.loadData(tm);
			this.statusResultLabel.setText("OK");
		} catch (PrologLoadException e) {
			this.statusResultLabel.setText("ERROR");
			e.printStackTrace();
		}
	}
}

package java_ui.steps;

import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;

import java_ui.prolog_loader.PrologLoadException;
import java_ui.prolog_loader.PrologLoader;
import java_ui.prolog_loader.RuleImportancePrologLoader;
import java_ui.table_editor.TableEditorDialog;
import java_ui.table_editor.panel.TableEditorPanel;
import java_ui.table_editor.panel.TableViewer;
import java.awt.GridBagLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import java.awt.GridBagConstraints;
import javax.swing.JButton;
import javax.swing.JCheckBox;

import java.awt.Insets;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.awt.event.ActionEvent;

public class DefineRuleImportanceStepPanel extends StepPanel{
	
	private JButton stepButton;
	private JLabel statusLabel;
	private JLabel statusResultLabel;
	
	private JCheckBox democracyCheckBox;
	
	private TableEditorPanel tep;
	private RuleImportancePrologLoader loader;
	private JButton viewButton;
	
	private TableViewer viewer;
	private JButton btnPriorityOrder;
	
	private DefineRuleImportanceStepPanel me;
	
	public DefineRuleImportanceStepPanel(String instruction, final String tableViewerTitle, final TableEditorPanel tep, final RuleImportancePrologLoader loader) {
		this.me = this; //'me' will be use as 'this' into ActionListeners.
		this.tep = tep;
		this.loader = loader;
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{45, 230, 129, 0};
		gridBagLayout.rowHeights = new int[]{0, 20, 0, 0};
		gridBagLayout.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
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
		
		btnPriorityOrder = new JButton("Agents priority order");
		btnPriorityOrder.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new DefineAgentPriorityOrderDialog(me).setVisible(true);
			}
		});
		
		GridBagConstraints gbc_btnPriorityOrder = new GridBagConstraints();
		gbc_btnPriorityOrder.anchor = GridBagConstraints.NORTH;
		gbc_btnPriorityOrder.fill = GridBagConstraints.HORIZONTAL;
		gbc_btnPriorityOrder.insets = new Insets(5, 5, 5, 5);
		gbc_btnPriorityOrder.gridx = 2;
		gbc_btnPriorityOrder.gridy = 0;
		add(btnPriorityOrder, gbc_btnPriorityOrder);
		
		
		JPanel containerStatus = new JPanel();
		GridBagConstraints gbc_containerStatus = new GridBagConstraints();
		gbc_containerStatus.anchor = GridBagConstraints.NORTH;
		gbc_containerStatus.fill = GridBagConstraints.HORIZONTAL;
		//gbc_containerStatus.insets = new Insets(5, 5, 5, 5);
		gbc_containerStatus.gridx = 0;
		gbc_containerStatus.gridy = 1;
		add(containerStatus, gbc_containerStatus);
		
		GridBagLayout gbl_containerStatus = new GridBagLayout();
		gbl_containerStatus.columnWidths = new int[]{45, 50, 50, 50, 0};
		//gbl_containerStatus.rowHeights = new int[]{70, 0};
		gbl_containerStatus.columnWeights = new double[]{1.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_containerStatus.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		containerStatus.setLayout(gbl_containerStatus);
		
		
		this.statusLabel = new JLabel("Status:");
		GridBagConstraints gbc_statusLabel = new GridBagConstraints();
		gbc_statusLabel.anchor = GridBagConstraints.NORTH;
		gbc_statusLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_statusLabel.insets = new Insets(0, 5, 5, 5);
		gbc_statusLabel.gridx = 0;
		gbc_statusLabel.gridy = 0;
		containerStatus.add(statusLabel, gbc_statusLabel);
		
		this.statusResultLabel = new JLabel("---");
		GridBagConstraints gbc_statusResultLabel = new GridBagConstraints();
		gbc_statusResultLabel.anchor = GridBagConstraints.NORTH;
		gbc_statusResultLabel.fill = GridBagConstraints.HORIZONTAL;
		gbc_statusResultLabel.insets = new Insets(0, 5, 5, 5);
		gbc_statusResultLabel.gridx = 1;
		gbc_statusResultLabel.gridy = 0;
		containerStatus.add(statusResultLabel, gbc_statusResultLabel);
		
		this.democracyCheckBox = new JCheckBox("Enable democratic defeat");
		GridBagConstraints gbc_democracyCheckBox = new GridBagConstraints();
		gbc_democracyCheckBox.anchor = GridBagConstraints.NORTH;
		gbc_democracyCheckBox.fill = GridBagConstraints.HORIZONTAL;
		gbc_democracyCheckBox.insets = new Insets(0, 5, 5, 5);
		gbc_democracyCheckBox.gridx = 0;
		gbc_democracyCheckBox.gridy = 2;
		add(democracyCheckBox, gbc_democracyCheckBox);
		
		this.democracyCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				loader.enableDemocraticDefeat(democracyCheckBox.isSelected());	
			}
		});
	
		viewButton = new JButton("View importance orders");
		viewButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				viewer = new TableViewer(tep.getTableModel());
				viewer.setFocusable(true);
				viewer.setTitle(tableViewerTitle);
				viewer.disableTable();
			}
		});
		
		this.stepButton = new JButton("Set importance orders");
		stepButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					TableModel backup = tep.getTableModel();
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
					else {
						tep.setTableModel(backup);
					}
				} 
				catch (PrologLoadException e1) {
					getFollowingStep().disableStep();
					statusResultLabel.setText("ERROR");
					e1.printStackTrace();
					JOptionPane.showMessageDialog(null, e1.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
					disableStep();
				}
				catch (IOException e1) {
					getFollowingStep().disableStep();
					statusResultLabel.setText("ERROR");
					e1.printStackTrace();
					disableStep();
				}
			}
		});
		GridBagConstraints gbc_stepButton = new GridBagConstraints();
		gbc_stepButton.fill = GridBagConstraints.HORIZONTAL;
		gbc_stepButton.anchor = GridBagConstraints.NORTH;
		gbc_stepButton.insets = new Insets(5, 5, 5, 5);
		gbc_stepButton.gridx = 2;
		gbc_stepButton.gridy = 1;
		add(stepButton, gbc_stepButton);
		GridBagConstraints gbc_viewButton = new GridBagConstraints();
		gbc_viewButton.fill = GridBagConstraints.HORIZONTAL;
		gbc_viewButton.insets = new Insets(5, 5, 5, 5);
		gbc_viewButton.anchor = GridBagConstraints.NORTH;
		gbc_viewButton.gridx = 2;
		gbc_viewButton.gridy = 2;
		add(viewButton, gbc_viewButton);
		
	}

	@Override
	public void enableStep() {
		this.stepButton.setEnabled(true);
		this.viewButton.setEnabled(true);
		this.btnPriorityOrder.setEnabled(true);
	}

	@Override
	public void disableStepAction() {
		this.stepButton.setEnabled(false);
		this.viewButton.setEnabled(false);
		this.btnPriorityOrder.setEnabled(false);
	}
	
	
	public void cleanStepAction(){
		this.tep.setTableModel(new DefaultTableModel());
		this.statusResultLabel.setText("---");
		if(this.viewer != null){
			this.viewer.setVisible(false);
			this.viewer = null;
		}
	}

	public PrologLoader.StatusCode getLoaderStatus(){
		return this.loader.getStatus();
	}
	
	public void setTableModel(TableModel tm) throws PrologLoadException{
		this.tep.setTableModel(tm);
		this.enableStep();
		try {
			this.loader.loadData(tm);
			this.statusResultLabel.setText("OK");
		} catch (PrologLoadException e) {
			this.statusResultLabel.setText("ERROR");
			JOptionPane.showMessageDialog(null, e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
			getFollowingStep().disableStep();
			throw e;
		}
	}
	
	public void setAgentsPriorityOrder(File file) {
		this.loader.setAgentPriorityOrder(file);
	}
}

package java_ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.table.TableModel;

public abstract class TableEditorPanel extends JPanel {
	
	protected JTable table;
	protected JScrollPane scrollPane;
	protected JButton addButton;
	protected JButton editButton;
	protected JButton removeButton;
	protected JPanel filePanel;
	protected JSeparator separator;
	protected JLabel fileLabel;
	protected JTextField fileText;
	protected JButton fileLoadButton;
	
	protected void init(){
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{257, 0};
		gridBagLayout.rowHeights = new int[]{124, 0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		this.setLayout(gridBagLayout);
		
		this.table = new JTable();
		this.table.setFillsViewportHeight(true);
		this.table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		
		this.scrollPane = new JScrollPane(this.table);
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.insets = new Insets(0, 0, 5, 0);
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 0;
		add(scrollPane, gbc_scrollPane);
		
		/*
		
		JPanel buttonsPanel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 1;
		add(buttonsPanel, gbc_panel);
		
		this.addButton = new JButton("Add");
		this.addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				addButtonAction(arg0);
			}
		});
		buttonsPanel.add(this.addButton);
		
		this.editButton = new JButton("Edit");
		this.editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				editButtonAction(arg0);
			}
		});
		buttonsPanel.add(this.editButton);
		
		this.removeButton = new JButton("Remove");
		this.removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				removeButtonAction(arg0);
			}
		});
		buttonsPanel.add(this.removeButton);
		
		this.separator = new JSeparator();
		GridBagConstraints gbc_separator = new GridBagConstraints();
		gbc_separator.insets = new Insets(0, 0, 5, 0);
		gbc_separator.gridx = 0;
		gbc_separator.gridy = 2;
		add(this.separator, gbc_separator);
		
		*/
		
		this.filePanel = new JPanel();
		GridBagConstraints gbc_filePanel = new GridBagConstraints();
		gbc_filePanel.insets = new Insets(0, 5, 5, 5);
		gbc_filePanel.fill = GridBagConstraints.BOTH;
		gbc_filePanel.gridx = 0;
		gbc_filePanel.gridy = 3;
		add(this.filePanel, gbc_filePanel);
		GridBagLayout gbl_filePanel = new GridBagLayout();
		gbl_filePanel.columnWidths = new int[]{0, 0, 0, 0};
		gbl_filePanel.rowHeights = new int[]{0, 0};
		gbl_filePanel.columnWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		gbl_filePanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		this.filePanel.setLayout(gbl_filePanel);
		
		this.fileLabel = new JLabel("File:");
		GridBagConstraints gbc_fileLabel = new GridBagConstraints();
		gbc_fileLabel.insets = new Insets(5, 5, 0, 5);
		gbc_fileLabel.gridx = 0;
		gbc_fileLabel.gridy = 0;
		this.filePanel.add(this.fileLabel, gbc_fileLabel);
		
		this.fileText = new JTextField();
		this.fileText.setEnabled(true);
		this.fileText.setEditable(false);
		this.fileText.setText("");
		GridBagConstraints gbc_fileText = new GridBagConstraints();
		gbc_fileText.insets = new Insets(5, 0, 0, 5);
		gbc_fileText.fill = GridBagConstraints.HORIZONTAL;
		gbc_fileText.gridx = 1;
		gbc_fileText.gridy = 0;
		this.filePanel.add(this.fileText, gbc_fileText);
		this.fileText.setColumns(10);
		
		this.fileLoadButton = new JButton("Load");
		this.fileLoadButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				try {
					String path = fileLoadButtonAction(arg0);
					
					if(path != null){
						fileText.setText(path);
					}
					
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		});
		
		GridBagConstraints gbc_fileLoadButton = new GridBagConstraints();
		gbc_fileLoadButton.insets = new Insets(5, 0, 0, 0);
		gbc_fileLoadButton.gridx = 2;
		gbc_fileLoadButton.gridy = 0;
		this.filePanel.add(this.fileLoadButton, gbc_fileLoadButton);
				
	}

	public void setTableModel(TableModel model){
		
		
		this.table.setModel(model);

		
		for(int i = 0; i < this.table.getColumnCount(); i++){
			this.table.getColumnModel().getColumn(i).setCellRenderer(new NoWrappingTableCellRenderer());
		}
		
	}
	
	public TableModel getTableModel() {
		return this.table.getModel();
	}
	
	protected abstract void addButtonAction(ActionEvent event);
	
	protected abstract void editButtonAction(ActionEvent event);
	
	protected abstract void removeButtonAction(ActionEvent event);
	
	protected abstract String fileLoadButtonAction(ActionEvent event) throws IOException;
}

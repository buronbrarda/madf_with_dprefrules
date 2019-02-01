package java_ui;

import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import java_ui.DefineCPrefRuleDialog.DefineCprefRuleEditingMode;

import javax.swing.JScrollPane;
import javax.swing.JButton;
import java.awt.GridBagLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class CriteriaEditorPanel extends JPanel {
	private JTable table;
	/**
	 * Create the panel.
	 */
	public CriteriaEditorPanel() {
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{257, 0};
		gridBagLayout.rowHeights = new int[]{124, 0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		table = new JTable();
		table.setFillsViewportHeight(true);
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.setModel(new DefaultTableModel(
			new Object[][] {
				{"c1", "[vbad, bad, reg, good, vgood]"},
				{"c2", "[vbad, bad, reg, good, vgood]"},
				{"c3", "[vbad, bad, reg, good, vgood]"},
			},
			new String[] {
				"Criterion", "Domain (Assessment Values)"
			}
		) {
			Class[] columnTypes = new Class[] {
				String.class, Object.class
			};
			public Class getColumnClass(int columnIndex) {
				return columnTypes[columnIndex];
			}
			boolean[] columnEditables = new boolean[] {
				false, true
			};
			public boolean isCellEditable(int row, int column) {
				return columnEditables[column];
			}
		});
		table.getColumnModel().getColumn(0).setPreferredWidth(60);
		table.getColumnModel().getColumn(1).setPreferredWidth(197);
		
		JScrollPane scrollPane = new JScrollPane(table);
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.insets = new Insets(0, 0, 5, 0);
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 0;
		add(scrollPane, gbc_scrollPane);
		
		JPanel panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 1;
		add(panel, gbc_panel);
		
		JButton btnNewButton_1 = new JButton("Add");
		btnNewButton_1.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				new DefineCPrefRuleDialog(DefineCprefRuleEditingMode.NEW,"r0");
			}
		});
		panel.add(btnNewButton_1);
		
		JButton btnNewButton_2 = new JButton("Edit");
		btnNewButton_2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				
				String selectedRule = (String) table.getModel().getValueAt(table.getSelectedRow(), 0);
				new DefineCPrefRuleDialog(DefineCprefRuleEditingMode.EDIT,selectedRule);
			}
		});
		panel.add(btnNewButton_2);
		
		JButton btnNewButton = new JButton("Remove");
		panel.add(btnNewButton);

	}
	
	
	//This custom renderer is needed to avoid text wrapping on rules table.
	private class NoTextWrappingRenderer extends JTextArea implements TableCellRenderer{

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {
			
			String text = (String) value;
			this.setText(text);
            this.setLineWrap(false);
            
            if (isSelected){
            	this.setBackground(new Color(188, 203, 226));
            }
            else{
            	this.setBackground(Color.WHITE);	
            }
            
            return this;
		}	
	}

}

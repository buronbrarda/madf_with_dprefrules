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

public class CPrefRulesEditorPanel extends JPanel {
	private JTable table;
	/**
	 * Create the panel.
	 */
	public CPrefRulesEditorPanel() {
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
				{"r1", "better(X,Y,c1) ==> preferred(X,Y)"},
				{"r2", "better(X,Y,c2), equal(X,Y,c1) ==> preferred(X,Y)"},
				{"r3", "better(X,Y,c3), equal(X,Y,c2), equal(X,Y,c1) ==> preferred(X,Y)"},
				{"r4", "better(X,Y,c3), min(X,c3,good), worse(X,Y,c2), max(Y,c2,good), equal(X,Y,c1) ==> preferred(X,Y)"},
			},
			new String[] {
				"Id", "Rule"
			}
		) {
			Class[] columnTypes = new Class[] {
				String.class, String.class
			};
			public Class getColumnClass(int columnIndex) {
				return columnTypes[columnIndex];
			}
			boolean[] columnEditables = new boolean[] {
				false, false
			};
			public boolean isCellEditable(int row, int column) {
				return columnEditables[column];
			}
		});
		
		table.getColumnModel().getColumn(0).setPreferredWidth(24);
		table.getColumnModel().getColumn(0).setCellRenderer(new NoTextWrappingRenderer());
		table.getColumnModel().getColumn(1).setPreferredWidth(497);
		table.getColumnModel().getColumn(1).setCellRenderer(new NoTextWrappingRenderer());
		
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

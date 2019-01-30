package java_ui;

import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import javax.swing.JScrollPane;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.UIManager;
import java.awt.GridLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class CPrefRulesEditor extends JPanel {
	private JTable table;
	private JTable table_1;
	private JTable table_2;

	/**
	 * Create the panel.
	 */
	public CPrefRulesEditor() {
		setLayout(new GridLayout(0, 1, 0, 0));
		
		JScrollPane scrollPane = new JScrollPane();
		add(scrollPane);
		
		table = new JTable();
		table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		table.setModel(new DefaultTableModel(
			new Object[][] {
				{"r1", "better(X,Y,c1) ==> preferred(X,Y)"},
				{"r2", "better(X,Y,c2), equal(X,Y,c1) ==> preferred(X,Y)"},
				{"r3", "better(X,Y,c3), equal(X,Y,c2), equal(X,Y,c1) ==> preferred(X,Y)"},
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
		table.getColumnModel().getColumn(0).setPreferredWidth(39);
		table.getColumnModel().getColumn(1).setPreferredWidth(544);
		
		scrollPane.setViewportView(table);
		
		JPanel panel = new JPanel();
		add(panel);
		
		JButton btnNewButton_1 = new JButton("Add");
		panel.add(btnNewButton_1);
		
		JButton btnNewButton_2 = new JButton("Edit");
		panel.add(btnNewButton_2);
		
		JButton btnNewButton = new JButton("Remove");
		panel.add(btnNewButton);
		

	}

}

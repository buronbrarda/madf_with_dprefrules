package java_ui;

import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.JScrollPane;
import javax.swing.JButton;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;

public class CPrefRulesEditorPanel extends JPanel {
	private JTable table;
	private JTable table_1;
	private JTable table_2;

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
		
		JScrollPane scrollPane = new JScrollPane();
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.insets = new Insets(0, 0, 5, 0);
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 0;
		add(scrollPane, gbc_scrollPane);
		
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
		table.getColumnModel().getColumn(1).setPreferredWidth(497);
		
		scrollPane.setViewportView(table);
		
		JPanel panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 1;
		add(panel, gbc_panel);
		
		JButton btnNewButton_1 = new JButton("Add");
		panel.add(btnNewButton_1);
		
		JButton btnNewButton_2 = new JButton("Edit");
		panel.add(btnNewButton_2);
		
		JButton btnNewButton = new JButton("Remove");
		panel.add(btnNewButton);
		

	}

}

package java_ui.table_editor.panel;

import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.table.TableModel;

import java_ui.table_editor.NoWrappingTableCellRenderer;

import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;

public class TableViewer extends JDialog {
	
	protected JPanel contentPane;
	protected JTable table;
	protected JScrollPane scrollPane;

	/**
	 * Create the frame.
	 */
	public TableViewer(TableModel tm) {
		
		setBounds(100, 100, 509, 170);
		this.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		
		this.table = new JTable(tm);
		this.table.setFillsViewportHeight(true);
		this.table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		
		//Define no-text-wrapping renderer
		for(int i = 0; i < this.table.getColumnCount(); i++){
			this.table.getColumnModel().getColumn(i).setCellRenderer(new NoWrappingTableCellRenderer());
		}
		
		JScrollPane scrollPane = new JScrollPane(table);
		
		this.getContentPane().add(scrollPane);
		
		
		this.setModalityType(ModalityType.MODELESS);
		this.setVisible(true);
	}

	public void disableTable() {
		this.table.setEnabled(false);	
	}

}

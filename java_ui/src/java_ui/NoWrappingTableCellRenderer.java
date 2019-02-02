package java_ui;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.table.TableCellRenderer;


//This custom renderer is needed to avoid text wrapping on rules table.
public class NoWrappingTableCellRenderer extends JTextArea implements TableCellRenderer{

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

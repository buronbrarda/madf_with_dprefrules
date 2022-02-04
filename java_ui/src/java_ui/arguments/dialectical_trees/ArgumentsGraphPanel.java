package java_ui.arguments.dialectical_trees;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.ISOMLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode;
import edu.uci.ics.jung.visualization.decorators.EllipseVertexShapeTransformer;
import edu.uci.ics.jung.visualization.renderers.DefaultVertexLabelRenderer;
import java_ui.arguments.Argument;
import java_ui.graphs.alternatives.lattice.Utils;

public class ArgumentsGraphPanel extends JPanel {
	
	private ArgumentsGraph graph;
	private ISOMLayout<Argument, ArgumentsGraphEdge> layout;
	private VisualizationViewer<Argument, ArgumentsGraphEdge> vv;
	private DefaultModalGraphMouse<Argument, ArgumentsGraphEdge> mouse;
	
	private final Color acceptedArgumentColor = new Color(31, 221, 19);
	private final Color acceptedSelectedArgumentColor = new Color(17, 117, 10);
	
	private final Color rejectedArgumentColor = new Color(243, 12, 12);
	private final Color rejectedSelectedArgumentColor = new Color(195, 9, 9);
	
	private final Color selectedArgumentLabelColor = new Color(3, 98, 252);
			
	private String selectedArgId;
	private DeltaExplanationPanel explanationPanel = null;
	
	
	public ArgumentsGraphPanel(){
		this.graph = new ArgumentsGraph();
		setLayout(new BorderLayout());
		this.mouse = new DefaultModalGraphMouse<Argument, ArgumentsGraphEdge>();
		
	}
	
	public ArgumentsGraphPanel(ArrayList<Argument> arguments){
		this.graph = new ArgumentsGraph(arguments);
		setLayout(new BorderLayout());
		this.mouse = new DefaultModalGraphMouse<Argument, ArgumentsGraphEdge>();
		
	}


	private void initVisualizerViewer(){
		this.selectedArgId = "";
		
		this.vv = new VisualizationViewer<Argument, ArgumentsGraphEdge>(this.layout);
		
		this.vv.addFocusListener(new ArgumentsGraphFocusListener());
		
		this.vv.setGraphMouse(this.mouse);
		
		
		JMenu menu = this.mouse.getModeMenu();
		menu.setText("MOUSE MODE");
		menu.setPreferredSize(new Dimension(200, 20));
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(menu);
		
		this.add(menuBar, BorderLayout.NORTH);
		
		this.mouse.setMode(Mode.PICKING);
		
		
		
		
		this.vv.getRenderContext().setEdgeLabelTransformer(new Transformer<ArgumentsGraphEdge, String>(){
				
			@Override
			public String transform(ArgumentsGraphEdge edge){
				return edge.getExplanation();
			}
			
		});
	
	
		this.vv.getRenderContext().setVertexLabelTransformer(new Transformer<Argument, String>(){

			@Override
			public String transform(Argument vertex) {
				
				String label = "";
				
				if(vertex != null){
					label += "A"+vertex.getId()+":<"+vertex.getRule().toString()+","+vertex.getClaim()+">";
				}
				
				return label;
			}
			
		});
		
		this.vv.getRenderContext().setEdgeFontTransformer(new ArgumentsGraphEdgeFontTransformer());
		this.vv.getRenderContext().setEdgeStrokeTransformer(new ArgumentsGraphEdgeStrokeTransformer());
		this.vv.getRenderContext().setEdgeDrawPaintTransformer(new ArgumentsGraphEdgeDrawPaintTransformer());
		this.vv.getRenderContext().setVertexFillPaintTransformer(new ArgumentFillPaintTransformer());
		this.vv.getRenderContext().setVertexShapeTransformer(new ArgumentShapeTransformer());
		this.vv.getRenderContext().setVertexStrokeTransformer(new ArgumentStrokeTransformer());
		this.vv.getRenderContext().setVertexLabelRenderer(new ArgumentLabelRenderer(selectedArgumentLabelColor));
		this.vv.getRenderContext().setVertexFontTransformer(new ArgumentFontTransformer());
		
		this.vv.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent arg0) {
				super.componentResized(arg0);
				Utils.fitGraph(vv, layout);
			}});
		
		
		this.vv.getPickedVertexState().addItemListener(new ItemListener() {
					
					@Override
					public void itemStateChanged(ItemEvent e) {
						if(vv.getPickedVertexState().getPicked().size() == 1) {
							Argument node = (Argument) e.getItem();
							
							if(vv.getPickedVertexState().isPicked(node)) {
								String argId = node.getId();
								setSelectedArgument(argId);
								if(explanationPanel != null)
									explanationPanel.setSelectedArgument(argId);
							}
							
							if(vv.getPickedVertexState().getPicked().isEmpty()) {
								setSelectedArgument("none");
								if(explanationPanel != null)
									explanationPanel.setSelectedArgument("none");
							}
						}
						else {
							setSelectedArgument("many");
							if(explanationPanel != null)
								explanationPanel.setSelectedArgument("none");
						}
						
					}
		});
	}
	
	public void loadGraph(){
		this.graph.load();
		
		this.layout = new  ISOMLayout<Argument, ArgumentsGraphEdge>(this.graph.getGraph());
		
		if(this.vv != null){
			this.remove(this.vv);
		}
			
		initVisualizerViewer();
		
		this.add(this.vv, BorderLayout.CENTER);
	}
	
	public void clearGraph(){
		this.graph.clearGraph();
		this.vv.repaint();
	}


	public void setSelectedArgument(String id) {
		this.selectedArgId = id;
		this.vv.repaint();
	}
	
	public void setExplanationPanel(DeltaExplanationPanel panel) {
		this.explanationPanel  = panel;
	}
	
	private class ArgumentsGraphFocusListener implements FocusListener {
		
		@Override
		public void focusLost(FocusEvent e) {
			// do nothing
		}
		
		@Override
		public void focusGained(FocusEvent e) {
			try {
				for(Argument v : vv.getPickedVertexState().getPicked()) {
					vv.getPickedVertexState().pick(v, false);
				}
			}catch (ConcurrentModificationException exception) {
				// do nothing
			}
		}
	}
	
	private class ArgumentsGraphEdgeStrokeTransformer implements Transformer<ArgumentsGraphEdge,Stroke>{

		@Override
		public Stroke transform(ArgumentsGraphEdge edge) {
			Stroke toReturn = null;
			
			//If it is a defeat the line must be solid.
			if(edge.isSuccessful()) {
				toReturn = new BasicStroke();
			}
			//Else, the line must be dashed
			else {
				float [] dash = {5.0f};
				toReturn = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash, 0.0f);
			}
			
			
			return toReturn;
		}
		
	}
	
	private class ArgumentsGraphEdgeFontTransformer implements Transformer<ArgumentsGraphEdge, Font>{
		
		private Font font = new Font(Font.SANS_SERIF , Font.PLAIN, 14);
		
		@Override
		public Font transform(ArgumentsGraphEdge edge) {
            
            return font;
        }
		
	}
	
	private class ArgumentsGraphEdgeDrawPaintTransformer implements Transformer<ArgumentsGraphEdge, Paint>{
		
		private final Color defeated = Color.BLACK;
		private final Color undefeated = new Color(120,120,120);
		
		@Override
		public Paint transform(ArgumentsGraphEdge edge) {
			return edge.isSuccessful()? defeated : undefeated;
		}
		
	}
	
	private class ArgumentFillPaintTransformer implements Transformer<Argument,Paint>{

		@Override
		public Color transform(Argument v) {
			if(selectedArgId.equals("many") && vv.getRenderContext().getPickedVertexState().isPicked(v)) {
				return selectedArgumentLabelColor;
			}
			else {
				if(v.getId().equals(selectedArgId)) {
					return v.isAccepted() ? acceptedSelectedArgumentColor : rejectedSelectedArgumentColor;
				}
				else {
					return v.isAccepted() ? acceptedArgumentColor : rejectedArgumentColor;
				}
			}
		}
		
	}
	
	private class ArgumentShapeTransformer extends EllipseVertexShapeTransformer<Argument>{
		
		ArgumentShapeTransformer() {
            setSizeTransformer(new Transformer<Argument, Integer>() {
            	
            	public Integer transform(Argument v){
            		return 30;
            	}
			});
        }
		
		public Shape transform(Argument v) {
            return factory.getRegularPolygon(v, 3);
        }
	}
	
	private class ArgumentStrokeTransformer implements Transformer<Argument, Stroke> {

		@Override
		public Stroke transform(Argument v) {
			float [] dash = {5.0f};
			Stroke toReturn = new BasicStroke();
			
			if(v.getId().equals(selectedArgId))
				toReturn = new BasicStroke(2.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash, 0.0f);
			
			return  toReturn;
		}
		
	};
	
	private class ArgumentLabelRenderer extends DefaultVertexLabelRenderer{

		public ArgumentLabelRenderer(Color pickedVertexLabelColor) {
			super(pickedVertexLabelColor);
		}
		
		@Override
		public <V> Component getVertexLabelRendererComponent(JComponent vv, Object value, Font font, boolean isSelected, V vertex) {
			isSelected = selectedArgId.equals("many") ? isSelected : false;
			super.getVertexLabelRendererComponent(vv, value, font, isSelected, vertex);
			
			if(((Argument)vertex).getId().equals(selectedArgId)) {
				this.setForeground(this.pickedVertexLabelColor);
			}
			
			
			return this;
		}
		
		
	}
	
	private class ArgumentFontTransformer implements Transformer<Argument, Font>{
		
		private Font font = new Font(Font.SANS_SERIF, Font.BOLD, 14);
		
		@Override
		public Font transform(Argument vertex) {
            
            return font;
        }
		
	}
}

/*
* Copyright 2009-2010 Gabriel Skantze.
* All Rights Reserved.  Use is subject to license terms.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer. 
* 
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in
*    the documentation and/or other materials provided with the
*    distribution.
* 
* 3. Original authors' names are not deleted.
*
* 4. The authors' names are not used to endorse or promote products
*    derived from this software without specific prior written
*    permission.
* 
* This work was supported by funding from KTH (Royal Institute of 
* Technology), Stockholm, Sweden.
* 
* KTH AND THE CONTRIBUTORS TO THIS WORK
* DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
* KTH NOR THE CONTRIBUTORS BE LIABLE FOR
* ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
* OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*/

package java_ui.graphs.alternatives.lattice;

import java.awt.Rectangle;
import java.awt.geom.Point2D;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;

public class Utils {
	
	public static final int HORIZONTAL_TREE_LAYOUT = 0;
	public static final int TREE_LAYOUT = 1;

	private static final double crossover = 1.0;
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static Rectangle getGraphBounds(Layout layout) {
		int minX = 10000;
		int minY = 10000;
		int maxX = 0;
		int maxY = 0;
		
		Graph graph = layout.getGraph();

		for (Object m : graph.getVertices()) {
			Point2D point = (Point2D) layout.transform(m);
			if (point != null) {
				minX = (int) Math.min(point.getX(), minX);
				minY = (int) Math.min(point.getY(), minY);
				maxX = (int) Math.max(point.getX(), maxX);
				maxY = (int) Math.max(point.getY(), maxY);
			}
		}
		
		return new Rectangle(minX, minY, maxX - minX, maxY - minY);
	}

	public static void centerGraph(VisualizationViewer<?,?> viewer, Layout<?,?> layout, double centerX, double centerY) {
	
		Rectangle r = getGraphBounds(layout);
		
		double midX = r.getX() + r.getWidth() / 2;
		double midY = r.getY() + r.getHeight() / 2;
		
		MutableTransformer modelTransformer = viewer.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT);
		
		modelTransformer.setTranslate(centerX - midX * modelTransformer.getScaleX(), centerY - midY * modelTransformer.getScaleY());

	}
	
	public static void scaleGraph(VisualizationViewer<?,?> vv, double newscale, Point2D at) {
	    
	    MutableTransformer layoutTransformer = vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT);
	    MutableTransformer viewTransformer = vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW);
	    
        if (newscale < crossover) {
        	viewTransformer.setScale(newscale, newscale, at);
	        layoutTransformer.setScale(crossover, crossover, at);
	    } else {
	    	layoutTransformer.setScale(newscale, newscale, at);
	        viewTransformer.setScale(crossover, crossover, at);
	    }
        
	}
	
	public static void fitGraph(VisualizationViewer<?,?> viewer, Layout<?,?> layout, double width, double height) {
		
		Rectangle r = getGraphBounds(layout);
		
		double midX = r.getX() + r.getWidth() / 2;
		double midY = r.getY() + r.getHeight() / 2;
		
		double scale = 1.0;
		if (r.getWidth() > 0 && r.getHeight() > 0) {
			scale = Math.min((width-100)/r.getWidth(), (height-100)/r.getHeight());
		} else if (r.getWidth() > 0) {
			scale = (width-100)/r.getWidth();
		} else if (r.getHeight() > 0) {
			scale = (height-100)/r.getHeight();
		}
				
		scaleGraph(viewer, scale, new Point2D.Double(0, 0));
		
		MutableTransformer modelTransformer = viewer.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT);
		
		if (scale > 1) 
			modelTransformer.setTranslate(width/2 - (midX * scale), height/2 - (midY * scale));
		else
			modelTransformer.setTranslate(width/(2*scale) - midX, height/(2*scale) - midY);
		
		viewer.repaint();
				
	}

	public static void fitGraph(VisualizationViewer<?,?> viewer, Layout<?,?> layout) {
		Rectangle r = viewer.getBounds();
		fitGraph(viewer, layout, r.getWidth(), r.getHeight());
	}
	
	public static void fitGraph(VisualizationViewer<?,?> viewer) {
		Rectangle r = viewer.getBounds();
		fitGraph(viewer, viewer.getModel().getGraphLayout(), r.getWidth(), r.getHeight());
	}
	
}

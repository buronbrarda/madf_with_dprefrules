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

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.apache.commons.collections15.Transformer;
import org.apache.commons.collections15.map.LazyMap;


import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.Graph;

public class JungLatticeLayout<V,E> implements Layout<V,E> {
	
	protected Dimension size = new Dimension(600,600);
	protected JungLatticeGraph<V,E> graph;

	private HashSet<V> done = new HashSet<V>();
	
    protected Map<V, Point2D> locations = 
    	LazyMap.decorate(new HashMap<V, Point2D>(),
    			new Transformer<V,Point2D>() {
					public Point2D transform(V arg0) {
						return new Point2D.Double();
					}});
	
    public JungLatticeLayout (JungLatticeGraph<V,E> graph) {
    	setGraph(graph);
    	this.update();
    }
    
    public void update() {
    	done.clear();
    	int y = 0;
    	for (V v : graph.getEnds()) {
    		y = updateRec(0, y, v);
    		y += 50;
    	}
    }
    
    public int updateRec(int x, int y, V v) {
    	int starty = y;
    	if (!done.contains(v)) {
			done.add(v);
			int i = 0;
			if (graph.getPredecessors(v) != null) {
				for (V sv : graph.getPredecessors(v)) {
					if (i > 0) y += 50;
					y = updateRec(x + 50, y, sv);
					i++;
				}
			}
			setLocation(v, new Point2D.Double(-x, starty + (y - starty) / 2));
    	}
    	return y;
    }
    
	@Override
	public Graph<V,E> getGraph() {
		return graph;
	}

	@Override
	public Dimension getSize() {
		return size;
	}

	@Override
	public void initialize() {
	}

	@Override
	public boolean isLocked(V v) {
		return false;
	}

	@Override
	public void lock(V v, boolean state) {
	}

	@Override
	public void reset() {
	}

	@Override
	public void setGraph(Graph<V,E> graph) {
		this.graph = (JungLatticeGraph<V,E>) graph;
	}

	@Override
	public void setInitializer(Transformer<V, Point2D> initializer) {
	}

	@Override
	public void setLocation(V v, Point2D location) {
		locations.get(v).setLocation(location);
	}

	@Override
	public void setSize(Dimension d) {
		throw new UnsupportedOperationException("Size of LatticeLayout is automaically set");
	}

	@Override
	public Point2D transform(V v) {
		return locations.get(v);
	}

}

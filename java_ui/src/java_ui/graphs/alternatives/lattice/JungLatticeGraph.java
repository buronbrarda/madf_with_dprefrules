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

import java.util.Collection;
import java.util.HashSet;

import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.util.Pair;

public class JungLatticeGraph<V,E> extends DirectedSparseMultigraph<V,E> {
	
	private HashSet<V> roots = new HashSet<V>();
	private HashSet<V> ends = new HashSet<V>();
	
	@Override
	public boolean addEdge(E e, V v1, V v2) {
		boolean success = super.addEdge(e, v1, v2);
		checkRoot(v1);
		checkRoot(v2);
		return success;		
	};

	public void clear() {
		roots.clear();
		ends.clear();
		Collection<E> edges = super.getEdges();
		while (!edges.isEmpty()) {
			super.removeEdge(edges.iterator().next());
		}
		Collection<V> vertices = super.getVertices();
		while (!vertices.isEmpty()) {
			super.removeVertex(vertices.iterator().next());
		}
	}
	
	public void checkRoot(V vertex) {
		if (getPredecessors(vertex) == null || getPredecessorCount(vertex) == 0) {
			if (!roots.contains(vertex))
				roots.add(vertex);	
		} else if (roots.contains(vertex)) {
			roots.remove(vertex);
		}	
		if (getSuccessors(vertex) == null || getSuccessorCount(vertex) == 0) {
			if (!ends.contains(vertex))
				ends.add(vertex);	
		} else if (ends.contains(vertex)) {
			ends.remove(vertex);
		}			
	}
	
	@Override
	public boolean addVertex(V vertex) {
		boolean success = super.addVertex(vertex);
		checkRoot(vertex);
		return success;
	};
	
	@Override
	public boolean removeEdge(E edge) {
		Pair<V> endpoints = getEndpoints(edge);
		boolean success = super.removeEdge(edge);
		checkRoot(endpoints.getFirst());
		checkRoot(endpoints.getSecond());
		return success;
	}
	
	@Override
	public boolean removeVertex(V vertex) {
		boolean success = super.removeVertex(vertex);
		checkRoot(vertex);
		return success;
	}
	
	public HashSet<V> getRoots() {
		return roots;
	}
	
	public HashSet<V> getEnds() {
		return ends;
	}

}

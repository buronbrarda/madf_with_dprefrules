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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;


public abstract class LatticeIncrement extends Increment {
	
	protected LatticeVertex predVertex = null;
	protected LatticeVertex succVertex = null;
		
	public LatticeIncrement(LatticeVertex predVertex, LatticeVertex succVertex) {
		super();
		setPredVertex(predVertex);
		setSuccVertex(succVertex);
	}
	
	public LatticeIncrement() {
		super();
	}
	
	public LatticeIncrement(String id) {
		super(id);
	}

	public LatticeIncrement getBestPredecessor() {
		if (predVertex == null || predVertex.getPredecessors().size() == 0)
			return null;
		else {
			double maxconf = Double.MIN_VALUE;
			LatticeIncrement best = null;
			for (SameLevelLink<LatticeIncrement> link : predVertex.getPredecessors()) {
				if (link.getTarget().getConfidence() > maxconf) {
					best = link.getTarget();
					maxconf = best.getConfidence();
				}
			}
			return best;
		}
	}
	
	public LatticeIncrement getPredecessor() {
		if (predVertex == null || predVertex.getPredecessors().size() == 0)
			return null;
		else
			return predVertex.getPredecessors().get(0).getTarget();
	}
	
	public ArrayList<SameLevelLink<LatticeIncrement>> getPredecessors() {
		if (predVertex == null)
			return null;
		else
			return predVertex.getPredecessors();
	}
	
	/** 
	 * Backtracks all predecessor increments in linear order
	 * @return A list of all predecessors 
	 */
	public List<? extends LatticeIncrement> getPastPredecessors() {
		List<LatticeIncrement> result = new LinkedList<LatticeIncrement>();
		LatticeIncrement increment = this;
		while (increment != null) {
			result.add(0, increment);
			increment = increment.getPredecessor();
		}
		return result;
	}
	
	public ArrayList<SameLevelLink<LatticeIncrement>> getSuccessors() {
		if (succVertex == null)
			return null;
		else
			return succVertex.getSuccessors();
	}
	
	public LatticeVertex getPredVertex() {
		return predVertex;
	}
	
	public LatticeVertex getSuccVertex() {
		return succVertex;
	}
	
	public void setPredVertex(LatticeVertex predVertex) {
		if (predVertex == null)
			this.predVertex = null;
		else {
			if (predVertex == this.succVertex) {
				try {
					throw new Exception("Tried to set predVertex to the same as succVertex");
				} catch (Exception e) {
					e.printStackTrace();
				}
			} else {
				predVertex.addSuccessor(this);
				this.predVertex = predVertex;
			}
		}
	}
	
	public void setSuccVertex(LatticeVertex succVertex) {
		if (succVertex == null)
			this.succVertex = null;
		else {
			if (succVertex == this.predVertex) {
				try {
					throw new Exception("Tried to set succVertex to the same as predVertex");
				} catch (Exception e) {
					e.printStackTrace();
				}
			} else {
				succVertex.addPredecessor(this);
				this.succVertex = succVertex;
			}
		}
	}

	public boolean hasPredecessor() {
		return (predVertex != null && predVertex.getPredecessors().size() > 0);
	}
	
	public boolean hasSuccessor() {
		return (succVertex != null && succVertex.getSuccessors().size() > 0);
	}
	
}

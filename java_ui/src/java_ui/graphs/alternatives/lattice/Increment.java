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

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

public abstract class Increment{
	
	private Double confidence = 1.0; 
	protected final List<Increment> groundedIn = new LinkedList<Increment>();
	private String id;

	public static IncrementMonitor monitor = null;
	
	public Increment() {
		this.id = "";
		if (monitor != null)
			monitor.newIncrement(this);
	}	
	
	public Increment(String id) {
		this.id = id;
		if (monitor != null)
			monitor.newIncrement(this);
	}
	
	public void setConfidence(Double confidence) {
		this.confidence = confidence;
	}

	public Double getConfidence() {
		return confidence;
	}

	public synchronized void addGroundedIn(Increment increment) {
		if (!groundedIn.contains(increment))
			groundedIn.add(increment);
	}

	public List<? extends Increment> getGroundedIn() {
		return groundedIn;
	}
	
	public void copyGroundedIn(Increment increment) {
		for (Increment i : increment.getGroundedIn()) {
			addGroundedIn(i);
		}
	}
	
	public HashSet<? extends Increment> getGroundedIn(Class<? extends Increment> incrementClass) {
		HashSet<Increment> increments = new HashSet<Increment>();
		for (Increment i : getGroundedIn()) {
			if (incrementClass.isAssignableFrom(i.getClass())) {
				increments.add(i);
			} else {
				increments.addAll(i.getGroundedIn(incrementClass));
			}
		}
		return increments;
	}

	public String getId() {
		return id;
	}
	
}

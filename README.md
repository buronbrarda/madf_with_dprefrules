# Argumentation-Based Decision Support System with Conditional Preference Rules (ADDS with Cpref-rules)

This project is an implementation of a research line carried out by Martín E. Buron Brarda, Luciano H. Tamargo and Alejandro J. García, all of them members of the Institute for Computer Science and Engineering (ICIC, UNS-CONICET).

The application contains two main modules: An interface for the users (developed in Java) and the application core (developed in SWI-Prolog). So, for running our application you will need to install the Java Virtual Machine and the SWI-Prolog interpreter into your computer. The poryect have been tested on Windows 10 with Java Runtime Environment (JRE) 7 and SWI-Prolog 7.6.4 You can download the JRE 7 (64 bits) from: https://www.oracle.com/java/technologies/javase/javase7-archive-downloads.html and SWI-Prolog 7.6.4 (64 bits) from: https://www.swi-prolog.org/download/stable?show=all.

We have tested our application only on machines running Windows. However, since all the the code that we have developed is executed via-interpreter, none major problems should appear for running our application in different operative systems (Linux, MAC OS, etc.). You can download the whole code, or just download the application that was built for Windows x64.

Also, for running our application in Windows it is required to have the SWI-Prolog '/bin' directory added to the %PATH% environment variable. In the following link, there is an instruction manual to do this: https://docs.telerik.com/teststudio/features/test-runners/add-path-environment-variables.


# Launch the application

After downloading the file cpref_decision_system.zip, you can launch the application executing the file app.jar using your pre-installed JVM (see the explanation above to verify if you meet all the requirements to execute the application).

To use the application, proceed as follows:

Start the application, 
1. You can press "Load Example" button, and the application will automatically check that the all the loaded data is correct. 
4. You will see that the "status" of the 5 steps (left part) change to "OK".
5. Then the "Run" button (at the bottom) starts the computation of the selected alternatives.
6. In the right part of the app's window the assessment base (K in the paper) will appear.  
7. At the right (bottom) the computed selected alternatives for the loaded example will be shown. 
8. The "Explanation Graph" button open a window with the graph.
9. Note that the "View" button shows the actual data of the loaded example for each item. 

At the Explanation Graph window:
· Vertices represent alternatives.
· Vertices are labelled with the alternative name.
· Green vertices represent the computed selected alternatives (otherwise are red).
· An arc from v1 to v2 labelled R, means that alternative v1 is preferred to v2 based on the preference rules R = {r1, .., rn}.
· For zoom in/out: use mouse's wheel.
· Vertices can be dragged for your convenience. 
· More than one vertex can be picked to be dragged (they will turn to yellow).
·Right click on vertices will show a pop-up menu with two items: Expand and Add Extra-Edges.
·Expand will transform a compund vertex with multiple alternatives into multiple vertices.
·Add extra-edges will generate all edges among the selected vertices.
·Right click on edges will show the explain option, if selected an Delta-Explanation Graph for the selected edge will be showed.

At the Delta-Explanation Graph window:
·Triangles represent arguments.
·Arguments are labelled with their claim and cpref-rules.
·Green arguments represent they were marked as undefeated (defeated arguments are red).
·The relations among arguments display the diferent dialectical trees the justify the anwser of the framework.
·Arguments can be dragged for your convenience. 
·Arguments than one vertex can be picked to be dragged.

At both Explanation Graph and Delta-Explanation Graph windows you can find in the top-left corner a menu to change the mouse mode from "picking" to "transforming" and vice-versa.

To create a new example or modified an existing one:
1. See the folder "examples" for a reference of the format of the files that are needed.
2. Then, the new files can be loaded using the "Edit" button for each of the 5 elements at the left.

# Thanks to..

	-- JPL7 (https://jpl7.org/)
	-- JUNG (http://jung.sourceforge.net/)
	-- APACHE.COMMONS.COLLECTIONS (http://commons.apache.org/)
	
They make possible to create this application!

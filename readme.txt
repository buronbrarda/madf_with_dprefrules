---------------------------------------------------------------------------------------------------------
     Argumentation-Based Multi-Criteria Decision Support System with Conditional Preference Rules
---------------------------------------------------------------------------------------------------------

** WE HAVE ONLY RUN THIS APPLICATION ON 64 BITS MACHINES RUNNING WINDOWS 7/10 OS. SO, ALL INSTRUCTIONS
ARE GIVEN CONSIDERING THIS ARQUITECTURE.

** WE SPECIALLY THANK THE DEVELOPERS OF THE LIBRARIES THAT WE HAVE USED TO CREATE THIS APPLICATION:

		-- JPL7 (https://jpl7.org/)
		-- JUNG (http://jung.sourceforge.net/)
		-- APACHE.COMMONS.COLLECTIONS (http://commons.apache.org/)
		
	All these libraries are included into the folder "app_lib"


==========================================================================================================
Pre-requisites
==========================================================================================================
1. Having installed Jave Runtime Environment 8 (64 bits) or superior.
You can download it from: https://java.com/en/download/manual.jsp

2. Having installed SWI-Prolog 7.6.0 (64 bits) or superior.
You can download it from: http://www.swi-prolog.org/download/devel

3. Add to the system path the SWI-Prolog executable folder
(Usually, you can find it as 'C:/Program Files/swipl/bin').
In the following link, there is an instrucction manual to do this:
https://docs.telerik.com/teststudio/features/test-runners/add-path-environment-variables

==========================================================================================================

TO LAUNCH THE APPLICATION EXECUTE app.jar.

To try the examples explained along the paper, proceed as follows:

Start the application, 
1.At the top (left) you can select between Evidence Set 1 (Table 1 at the paper) or Evidence Set 2  (Table 2 at the paper). 
2.For each evidence set, three different set of preference rules can be selected (Tim, August and Kate respectively).
3.Once an evidence set and a set of preferences is selected (note Evidence set 1 and Tim’s preferences are selected by default), the “Load Example” button can be used, and the application will automatically check that the all the loaded data is correct. 
4.You will see that the “status” of the 5 steps (left part) change to “OK”.
5.Then the “Run” button (at the bottom) starts the computation of the selected alternatives.
6.In the right part of the app’s window the assessment base (K in the paper) will appear.  
7.At the right (bottom) the computed selected alternatives for the loaded example will be shown. 
8.The “Explanation Graph” button open a window with the graph.
9.Note that the “View” button shows the actual data of the loaded example for each item. 

At the Explanation Graph window:
•Vertices represent alternatives. 
•Vertices are labelled with the alternative name.
•White vertices represent the computed selected alternatives (otherwise are gray).
•An arc from v1 to v2 labelled R, means that alternative v1 is preferred to v2 based on the preference rules R = {r1….}.
•For zoom in/out: use mouse’s wheel.
•Vertices can be dragged for your convenience. 
•More than one vertex can be picked to be dragged (they will turn to yellow).
•Right click on vertices will show a pop-up menu with two items: Expand and Add Extra-Edges.
•Expand will transform a compund vertex with multiple alternatives into multiple vertices.
•Add extra-edges will generate all edges among the selected vertices.
•Right click on edges will show the explain option, if selected an Delta-Explanation Graph for the selected edge will be showed.


At the Delta-Explanation Graph window:
•Triangles represent arguments.
•Arguments are labelled with their claim and cpref-rules.
•Green arguments represent they were marked as undefeated (defeated arguments are red).
•The relations among arguments display the diferent dialectical trees the justify the anwser of the framework.
•Arguments can be dragged for your convenience. 
•Arguments than one vertex can be picked to be dragged.

At both Explanation Graph and Delta-Explanation Graph windows you can find in the top-left corner a menu to change the mouse mode from "picking" to "transforming" and vice-versa.

To create a new example or modified an existing one:
1. See the folder “Examples” for a reference of the format of the files that are needed.
2. Then, the new files can be loaded using the “Edit” button for each of the 5 elements at the left.


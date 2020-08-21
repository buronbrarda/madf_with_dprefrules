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
1. Having installed Jave Runtime Environment 7 (64 bits).
You can download it from: https://www.oracle.com/java/technologies/javase/javase7-archive-downloads.html

2. Having installed SWI-Prolog 7.6.4 (64 bits).
You can download it from: https://www.swi-prolog.org/download/stable?show=all 

3. Add to the system PATH environment variable the SWI-Prolog executable folder
(Usually, you can find it as 'C:/Program Files/swipl/bin').
In the following link, there is an instrucction manual to do this:
https://docs.telerik.com/teststudio/features/test-runners/add-path-environment-variables

==========================================================================================================

To execute the application just double click on app.jar, or type java -jar app.jar
in your command console.

In order to try our application with the default example you can press on Load example,
and then press on Run.

To create a new example or modified an existing one you can modify the file into the examples folder.
Also, you can edit the current data (criteria, evidence or cpref-rules) by loading a new .csv file.
To do this press the Edit button on the desired frame, and then load the file with your own data.


Explanation graph:
- Vertices represent alternatives or group of alternatives. 
- Vertices are labeled with the alternatives' id.
- White vertices represent the computed selected alternatives (otherwise are black).
- An arc from v1 to v2 labeled R, means that alternative v1 is preferred to v2 based on the set of rules R.
- For zoom in/out: use mouse wheel.
- Vertices can be dragged for your convenience. 
- More than one vertex can be picked to be dragged (they will turn to cyan when picked).
- Keep pressed shift to combine your current selection with a new one.
- Right click on vertices will display a pop-up menu with two items: Expand and Add Extra-Edges.
- Expand will transform a compund vertex with multiple alternatives into multiple vertices.
- Add extra-edges will generate all edges among the selected vertices.
- Right click on arcs will show the explain option, if selected the Delta-Explanation and Argument graph
for the selected arcs will be shown.


Delta-Explanation:
- Triangles represent arguments.
- Arguments are labeled with their ids.
- Blue arguments represent they were marked as undefeated (defeated arguments are yellow).
- The relations among arguments display the different dialectical trees the justify the anwser of the framework.
- Arguments can be dragged for your convenience.

Argument graph:
- Triangles represent arguments.
- Arguments are labeled with their ids, claim, and Cpref-rule.
- The relations among the arguments depicts the defeat relation.
- Green arguments represent they are accepted (rejected arguments are red).
- Arguments can be dragged for your convenience.

On graph windows you can find in the top-left corner a menu to change the mouse mode from "picking" to "transforming" and vice-versa.

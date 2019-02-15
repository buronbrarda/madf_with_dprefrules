# Argumentation-Based Multi-Criteria Decision Support System with Conditional Preference Rules

This project is an implementation of a research line carried out by Martin E. Buron Brarda, Luciano H. Tamargo and Alejandro J. García, all of them integrants of the Institute for Computer Science and Engineering (ICIC, UNS-CONICET).

The application contains two main modules: An interface for the users (developed in Java) and the application core (developed in SWI-Prolog). So, for running our application you will need to install the Java Virtual Machine and the SWI-Prolog interpreter into your computer. You can download the Jave Runtime Environment 8 (64 bits) or superior from: https://java.com/en/download/manual.jsp. SWI-Prolog 7.6.0 (64 bits) or superior can be downloaded from: http://www.swi-prolog.org/download/devel.

We have tested our application only on machines running Windows. However, since all the the code that we have developed is excuted via-interpreter, none major problems should appear for running our application in different operative systems (Linux, MAC OS, etc.). You can download the whole code, or just download the application that was built for Windows x64.

Also, for runnig our application in Windows it is required to have the local path for SWI-Prolog interprer added as a Global Path Variable. In the following link, there is an instrucction manual to do this: https://docs.telerik.com/teststudio/features/test-runners/add-path-environment-variables.


# Launch the application

After downloading the file cpref_decision_system.zip, you can launch the application excuting the file app.jar using your pre-installed JVM (see the explanation above to verify if you meet all the requisites to execute the application).

To use the application, proceed as follows:

Start the application, 
1. At the top (left) you can select between Evidence Set 1 or Evidence Set 2. 
2. For each evidence set, three different set of preference rules can be selected (Tim, August and Kate respectively).
3. Once an evidence set and a set of preferences is selected (note Evidence set 1 and Tim’s preferences are selected by default), the “Load Example” button can be used, and the application will automatically check that the all the loaded data is correct. 
4. You will see that the “status” of the 5 steps (left part) change to “OK”.
5. Then the “Run” button (at the bottom) starts the computation of the selected alternatives.
6. In the right part of the app’s window the assessment base (K in the paper) will appear.  
7. At the right (bottom) the computed selected alternatives for the loaded example will be shown. 
8. The “Explanation Graph” button open a window with the graph.
9. Note that the “View” button shows the actual data of the loaded example for each item. 

At the Explanation Graph window:
• Vertices represent alternatives. 
• Vertices are labelled with the alternative name.
• Green vertices represent the computed selected alternatives (otherwise are red).
• An arc from v1 to v2 labelled R, means that alternative v1 is preferred to v2 based on the preference rules R = {r1….}.
• For zoom in/out: use mouse’s wheel.
• Vertices can be dragged for your convenience. 
• More than one vertex can be picked to be dragged (they will turn to yellow).

To create a new example or modified an existing one:
1. See the folder “Examples” for a reference of the format of the files that are needed.
2. Then, the new files can be loaded using the “Edit” button for each of the 5 elements at the left.

PASSFINDER - A Java Satellite Pass Predictor
--------------------------------------------
 
Author : Pedro j. Fernández <pedrojfr1@wanadoo.es>
Date : 22/8/2004
Version : 0.1

--------------------------------------------


This is a demo application that uses the SGP4 Java port and shows how to
use it to do satellite pass predictions.


1. HOW TO EXECUTE:

In the passfinder directory, write this command 
(I put it for tou in PassFinder.bat) :

java -classpath ".\classes" PassFinderISS "<location name>" 
                                           <lat ºN> 
                                           <long ºE> 
                                           <alt m.>
                                           <UTC_Offset in Hours> 
                                           <how many days to predict> 
                                           <only shows passes which max. elevation higher than this elevation º>


Example of my location:

java -classpath ".\classes" PassFinderISS "Murcia City" 37.982 -1.1076 45 2 30 15 > InformeMurcia.html


NOTE: This program requires Internet connection for "stations.txt"
      file download from celestrak.com
      If you want to read "stations.txt" from disk, delete this line
      in "PassFinderISS.java":

      PassView.actualizacionForzadaTLE(true); // Forces to use Internet 

      Recompile it. Now if "stations.txt" is found locally, it will read 
      this form disk. If it not exists, then if will read this from 
      the Internet (celestrak.com).



You can find an example of prediction output in "InformeMurcia.html".
It is in spanish, but it isn't dificult to you to modify source code 
to translate it.

All the classes are already compiled.
PassFinder.jpx and PassFinder.jpx.local are the project files from JBuilder 9.
Use it to recompile source code if it is necesary. You are free to use your own
compilation method.

Enjoy it!
Batch Files: BdM2S2 
This zip file contains batch files to compile our data and run the results for each chapter. All files are for stata 7. 

The file Bdm2s2_Compile_nationYearData.do takes all the consistuent data and compiles them to create the dataset we use. 

The batch files all assume a particular directory structure. Towards the top of each batch file a directory is specified. Initially it is set to "cd c:\bdm2s2\data\May2002\"  which set the directory to "c:\bdm2s2\data\May2002\". This is where the program will look for files. We recommend with directort and placing all the batch files and the main nation-year data set in the directory. The constituent dataset should be placed in "c:\bdm2s2\data\May2002\datasets". Also generate a directory "c:\bdm2s2\data\May2002\output" where by default the batch file will generate a log of the results. By altering the "cd c:\bdm2s2\data\May2002\" command at the top of each batch file you can reorganise the directory structure. 

The batch file names indicate which chapter they are associated with. 
 


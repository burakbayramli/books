
SUPPLEMENTARY MATLAB MATERIAL FOR
---------------------------------

DISCRETE-TIME SPEECH SIGNAL PROCESSING: Principles and Practice
---------------------------------------------------------------

by Thomas F. Quatieri
---------------------

Located in this Prentice Hall web site compartment are functions, scripts, workspaces, 
and data required by the MATLAB exercises in the text: Discrete-Time Speech Sign
specific cases of a certain class of speech signal, i.e., the waveforms often can 
be replaced by other examples with similar characteristics that serve the instructive 
purpose of the exercise.

Each chapter has its own directory structure. As an example, the directory 
for Chapter 13 consists of:

chapter13/
---------

Backup and Data Directories:
---------------------------
Back/
Data/

Workspaces:
----------
ex11M1.mat          
ex11M2.mat          

Functions and Scripts:
---------------------
amfm_sep.m          
gaussian_filt.m     
wigner.m
specgram_speech.m
cross.m             
stftm.m

In addition, occasionally, readme files appear within the sub-directories. 
These readme files are in text format and describe or clarify contents of 
the sub-directory or a particular MATLAB exercise.

Speech files in the Data directory are in both pcm and wav format (but are 
also included in the given workspaces). The sampling rate of each speech 
file is indicated in the file name. The Data directory for Chapter 13, 
for example, contains the required speech files:

speech1_10k.wav   speech_10k.wav
speech1_10k.pcm   speech_10k.pcm   

If the designated sampling is not supported by a desired platform, 
then in order to listen to the files, a change in sampling rate must 
be made (e.g., using MATLAB decimation and interpolation functions) 
for the particular platform.




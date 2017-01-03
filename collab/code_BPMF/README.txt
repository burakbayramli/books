% Code provided by Ruslan Salakhutdinov
%
% Permission is granted for anyone to copy, use, modify, or distribute this
% program and accompanying programs and documents for any purpose, provided
% this copyright notice is retained and prominently displayed, along with
% a note saying that the original programs are available from our
% web page.
% The programs and documents are distributed without any warranty, express or
% implied.  As the programs were written for research purposes only, they have
% not been tested to the degree that would be advisable in any important
% application.  All use of these programs is entirely at the user's own risk.

How to make it work:

   1. Create a separate directory and download all these files into the same directory
   2. Download the following 7 files:
          * demo.m Main file for training PMF and Bayesian PMF
          * pmf.m  Training PMF model 
          * bayespmf.m  Bayesian PMF model that implements Gibbs sampler.  
          * moviedata.mat Sample data that contains triplets (user_id, movie_id, rating)
          * makematrix.m Helper function that converts triplets into large matrix. 
            This file is used by bayespmf.m 
          * pred.m Helper function that makes predictions on the validation set.  
          * README.txt

    3. Simply run demo.m in Matlab. It will fit PMF and then will run Bayesian PMF.   

This code uses Matlab stats toolbox to sample from Wishart distribution. 
If you don't have stats toolbox you can use Tom Minka's 
"The Lightspeed Matlab Toolbox" (just google it). 


I did not try to optimize this code, but please e-mail me if you find bugs.




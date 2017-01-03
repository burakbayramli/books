%            INTRODUCTION TO LINEAR ALGEBRA TOOLBOX
% 
% This is a package of MATLAB programs to use with MATLAB in learning
% and experimenting with linear algebra. The toolbox is coordinated with 
% the text
% 
%          Introduction to Linear Algebra   by  Gilbert Strang  (1993)
%          Wellesley-Cambridge Press   Box 812060   Wellesley MA 02181
% 
% The last page of the book lists the Teaching Codes which are the backbone of
% this Toolbox. Many programs are printed in the book - this diskette makes
% it easy to use them.  Each code comes with a read-only help file.
% 
% Each Teaching Code also has an "x file" to demonstrate how it can be used.
% You will see that the examples and experiments also lead to questions. 
% If necessary those can be called exercises. Their purpose is 
% entirely friendly - to suggest how to start using the codes.
% 
% The email contact for this Toolbox is linalg@math.mit.edu  This address will 
% respond to math questions. You may also request an email copy of the Toolbox. 
% Linalg is NOT COMPETENT to help with installation or computer questions.
% For MATLAB itself all questions should be directed to support@mathworks.com
% For the Student Version, Prentice-Hall gives support via matlab@prenhall.com
% The Toolbox is not prepared by The MathWorks so it cannot provide support.
% 
% This Toolbox is written to be used with MATLAB 3.5, 4.0, 4.1 and the Student
% Edition of MATLAB. [Some graphics commands are new in 4.0]  We mention the
% new SYMBOLIC MATH TOOLBOX based on Maple V which allows symbolic expressions
% instead of numbers. The command d = determ(A - x*I) gives a polynomial in x. 
% There is also variable precision arithmetic beyond the 16 digits in MATLAB.
% 
% Now we list the programs in this Toolbox. They are in three groups:
% 
%   (1)  Teaching Codes, each with help.
%   (2)  A explanatory "x" code each of the Teaching Codes
%   (3)  A growing collection of extra programs to illustrate linear algebra.
% 
% The Teaching Codes are:
% 
%   BASIS       EIGEN2        LSQ         PROJMAT         SLU
%   COFACTOR    FINDPIV       NULL        RANDPERM        SLV
%   CRAMER      GRAMS         PERMDET     RATS            SOLVE
%   DETERM      INVERSE       PLOT2D      REF             SPLU
%   EIGEN       LINEFIT       PLU         SIGNPERM        SPLV
% 
% The extra programs include:
% 
%    ADDVEC    EXPAGE35   HAND       FOURIER    POLY2STR   SIXPACK
%    ATIMESV   EXPAGE36   HOUSE      LINPROG    POWER      ZEROONE
%    COSINE    EXPOWER    FASTFOUR   MOVIES                
%   
% For a quick explanation of one of the Teaching Codes, for example basis, type
%    help basis
% 
% To see some examples and exercises involving BASIS, type
%    xbasis
% 
% To see the actual MATLAB code, type
%    type basis
% 
% To apply the function to a matrix A, type
%    basis(A)
% 
% Here is an index, showing the relevant section and page in the text
% for each code.
%
%  Section  Page
%     1.1     2    ADDVEC   
%     1.1     2    ATIMESV  
%     1.2    16    COSINE   
%     1.4    35    EXPAGE35 
%     1.4    36    EXPAGE36 
%     2.3    56    SIXPACK  
%     2.4    71    MOVIES   
%     2.4    72    INVERSE  
%     2.4    72    ZEROONE  
%     2.5    83    SLU      
%     2.5    83    SLV      
%     2.6    96    SPLU     
%     2.6    97    SPLV     
%     3.2   118    FINDPIV  
%     3.2   119    PLU      
%     3.2   119    REF      
%     3.2   120    NULL     
%     3.3   128    SOLVE    
%     3.4   143    BASIS    
%     4.2   172    PROJMAT  
%     4.3   179    LINEFIT  
%     4.3   179    LSQ      
%     4.4   194    GRAMS    
%     5.1   201    DETERM   
%     5.2   214    PERMDET  
%     5.2   214    RANDPERM 
%     5.2   214    SIGNPERM 
%     5.2   216    COFACTOR 
%     5.3   223    CRAMER   
%     6.1   237    EIGEN    
%     6.1   237    EIGEN2   
%     6.1   237    POLY2STR 
%     7.1   306    HAND     
%     7.1   306    HOUSE    
%     7.1   309    PLOT2D   
%     8.3   357    LINPROG  
%     9.3   395    EXPOWER  
%     9.3   395    POWER    
%    10.2   415    FOURIER  
%    10.3   421    FASTFOUR 
% 
% A good, quick introduction to MATLAB is provided by a 25-page tutorial,
%    The MATLAB Primer, by Kermit Sigmon.
% This is available by anonymous ftp from
%    math.ufl.edu   directory: pub/matlab  file: primer35.tex
% and also from
%    ftp.mathworks.com   directory: pub/doc/Primer  file: primer35.tex
% The Primer can be distributed via a local copy center. The ftp file is
% plain TEX.  For PostScript change the file name to primer.ps  There is
% also a Spanish edition. 
% 
% Suggestions for the Toolbox can be directed to linalg@math.mit.edu
% The email address to request this Toolbox is   info@mathworks.com

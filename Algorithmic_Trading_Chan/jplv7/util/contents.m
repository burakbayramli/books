% Utility function library -- Jim LeSage
%
%   
%       accumulate : accumulates column elements of a matrix x
%        blockdiag : Construct a block-diagonal matrix with the inputs on the diagonals.
%              cal : create a time-series calendar structure variable that
%            cal_d : An example of using cal()                               
%           ccorr1 : converts matrix to correlation form with unit normal scaling.
%           ccorr2 : converts matrix to correlation form with unit length scaling.
%             cols : return columns in a matrix x
%            crlag : circular lag function
%         cumprodc : compute cumulative product of each column
%          cumsumc : compute cumulative sum of each column
%            delif : select values of x for which cond is false
%           diagrv : replaces main diagonal of a square matrix
%            dmult : computes the product of diag(A) and B
%         find_big : finds rows where at least one element is > #
%        find_bigd : An example of using find_big()  
%         findnear : finds element in the input matrix (or vector) with 
%           fturns : finds turning points in a time-series
%         fturns_d : demo of fturns() 
%          growthr : converts the matrix x to annual growth rates
%             ical : finds observation # associated with a year,period 
%           ical_d : An example of using ical()                               
%         indexcat : Extract indices for y being equal to val if val is a scaler
%        indicator : converts the matrix x to indicator variables
%         invccorr : converts matrix to correlation form with
%            invpd : A dummy function to mimic Gauss invpd
%          invpd_d : An example of using invpd()                               
%         kernel_n : normal kernel density estimate
%              lag : creates a matrix or vector of lagged values
%           levels : produces a variable vector of factor levels 
%           lprint : print an (nobs x nvar) matrix in LaTeX table format
%         lprint_d : demo of lprint() 
%          lprintf : Prints a matrix of data with a criteria-based symbol next
%        lprintf_d : demo of lprintf() 
%    make_contents : makes pretty contents.m files for the Econometrics Toolbox
%        make_html : makes HTML verion of contents.m files for the Econometrics Toolbox
%           matadd : performs matrix addition even if matrices
%           matdiv : performs matrix quotient even if matrices
%           matmul : performs matrix multiplication even if matrices
%           matsub : performs matrix subtraction even if matrices
%             mlag : generates a matrix of n lags from a matrix (or vector)
%             mode : computes the mode of a vector x
%           mprint : print an (nobs x nvar) matrix in formatted form
%          mprint3 : Pretty-prints a set of matrices together by stacking the 
%        mprint3_d : An example of using mprint3
%         mprint_d : demo of mprint() 
%          mth2qtr : converts monthly time-series to quarterly averages
%            nclag : Generates a matrix of lags from a matrix containing
%              plt : Plots results structures returned by most functions
%            prodc : compute product of each column
%              prt : Prints results structures returned by most functions
%         recserar : computes a vector of autoregressive recursive series
%         recsercp : computes a recursive series involving products
%         roundoff : Rounds a number(vector) to a specified number of decimal places
%             rows : return rows in a matrix x
%             sacf : find sample autocorrelation coefficients 
%           sacf_d : demo of sacf() 
%            sdiff : generates a vector or matrix of lags
%           sdummy : creates a matrix of seasonal dummy variables
%            selif : select values of x for which cond is true
%             seqa : produce a sequence of values
%             seqm : produce a sequence of values
%            shist : spline-smoothed plot of a histogram
%            spacf : find sample partial autocorrelation coefficients 
%          spacf_d : demo of spacf() 
%             stdc : standard deviation of each column
%             sumc : compute sum of each column
%            tally : calculate frequencies of distinct levels in x
%            tdiff : produce matrix differences
%            trimc : return a matrix (or vector) x stripped of the specified columns.
%            trimr : return a matrix (or vector) x stripped of the specified rows.
%           tsdate : produce a time-series date string for an observation #
%         tsdate_d : demonstrate tsdate functions
%          tsprint : print time-series matrix or vector with dates and column labels
%        tsprint_d : Examples of using tsprint()                               
%           unsort : takes a sorted vector (or matrix) and sort index as input
%         unsort_d : demo of unsort() 
%           util_d : demonstrate some of the utility functions
%              vec : creates a column vector by stacking columns of x
%             vech : creates a column vector by stacking columns of x
%             vecr : creates a column vector by stacking rows of x
%            vprob : returns val = (1/sqrt(2*pi*he))*exp(-0.5*ev*ev/he)
%        xdiagonal : spreads an nxk observation matrix x out on
%          yvector : repeats an nx1 vector y n times to form

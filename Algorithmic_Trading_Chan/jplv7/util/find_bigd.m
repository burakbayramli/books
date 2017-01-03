% PURPOSE: An example of using find_big()  
%         to find rows in a matrix
%          that meet a size criterion
%                                  
%---------------------------------------------------
% USAGE: find_bigd
%---------------------------------------------------


tst = [1 2 3 4
       5 6 7 8
       9 10 11 12];

index = find_big(tst,4);
% should point to rows 2 and 3 where at
% least 1 row-element is > 4
index

tst = [1 2 3 4
       5 6 7 8
       9 10 4 12];

index = find_big(tst,9);
% should point to row 3 where at
% least 1 row-element is > 9
index


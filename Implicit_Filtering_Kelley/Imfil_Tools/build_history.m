function new_hist=build_history(xarray,funmat,failvec)
% BUILD_HISTORY
% Build a history structure after one or more calls to f. You will need this
% if your function needs to pass data to imfil.m for updating the
% complete_history structure.
%
% function new_hist=hist_update(old_hist,xarray,funmat,failvec)
%
% Input: xarray = array of points 
%       funmat = f(xarray); following the convention for parallel function
%                    evaluation in imfil, funmat(:,i) = f(xarray(:,i))
%                    so funmat is a row vector for optimization and
%                    a matrixarray for nonlinear least squares.
%       failvec = column vector of failure flags
%
% If the parallel option is on, funmat could be a matrixarray
% and we need to be prepared for that. The test is 
% a look at the size of failvec, which tells me how many
% columns funmat has.
%
[mout, nout] = size(failvec);
new_hist=struct('good_points',[],'good_values',[],'failed_points',[]);
%
% Identify the failed points and update the structure if there are any.
%
ibad=(failvec(1:nout)==1);
if sum(ibad) > 0
   new_hist.failed_points=xarray(:,ibad);
end
%
% Do the same for the good points.
%
igood=(failvec(1:nout)==0);
if sum(igood) > 0
    new_hist.good_points=xarray(:,igood);
    new_hist.good_values=funmat(:,igood);
end

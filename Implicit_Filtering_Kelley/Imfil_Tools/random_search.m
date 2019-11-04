function [xs, fs, my_cost, explore_history] = ...
       random_search(f_internal,iteration_data,my_search_data);
% RANDOM_SEARCH
% function [xs, fs, my_cost, explore_history] = ...
%      random_search(f_internal,iteration_data,my_search_data);
%
% This is an example of an explore_function.
% This function samples f at a few random points and returns the best 
% thing it found. I store the number of random points in the
% my_search_data structure.
%
options=iteration_data.options;
parallel = options.parallel;
imfil_complete_history=options.complete_history;
npoints=my_search_data;
xarray=rand(2,npoints);
farray=[];
my_cost=0;
%
% Extract what you need from the structures. 
%
% Pass h and core_data to f_internal
%
h=iteration_data.h;
core_data=iteration_data.core_data;
%
% What's the current best point and best objective function value?
%
xb=iteration_data.xb;
funsb=iteration_data.funsb;
fvalb=iteration_data.fobjb;
%
% Am I solving a least squares problem?
%
least_squares=iteration_data.options.least_squares;
%
% Sample the points. Keep the books for the build_history function.
%
failvec=zeros(1,npoints);
funmat=[];
if parallel == 0
   for i=1:npoints
      x=xarray(:,i);
      [funmati,failvec(i),icount,tol] = feval(f_internal,x,h,core_data);
      funmat=[funmat,funmati];
      my_cost=my_cost+icount;
   end
else
   [funmat,fail,icount]=feval(f_internal,xarray,h,core_data);
   my_cost = my_cost+sum(icount);
end
%
% Do the right thing for least squares problems.
%
for i=1:npoints
    if least_squares == 1
        fval=funmat(:,i)'*funmat(:,i)/2;
    else
        fval=funmat(i);
    end
    farray=[farray',fval]';
end

%
% Now see if you've made any progress. If not, return the 
% the current best point.
%
[ft,imin]=min(farray);
if failvec(imin) == 0
   xs=xarray(:,imin);
   fs=funmat(:,imin);
else
   fs=funsb;
   xs=xb;
end
%
% Finally, build the explore_data structure.
%
if imfil_complete_history == 1
   explore_history = build_history(xarray, funmat, failvec);
else
   explore_history=[];
end

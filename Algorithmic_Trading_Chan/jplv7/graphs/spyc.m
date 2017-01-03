function spyc(S,marker,markersize);
% PURPOSE: produces a spy plot with colors
% that reflect the magnitude of the non-zero numbers
% -----------------------------------------------------
% USAGE: spyc(S) or spyc(S,marker,markersize)


if nargin == 2
    markersize = 5;
elseif nargin == 1
    markersize = 5;
    marker = '.';
elseif nargin == 3
    % do nothing
else
    error('spyc: Wrong # of input arguments');
end;
[n,m] = size(S);
[i,j,ss] = find(S);  % Note we need the values of the nonzeros as 
                     % well as their indices
if isempty(i), i = NaN; j = NaN; end
if isempty(S), marker = 'none'; end
scatter(i,j,markersize,ss,marker);
xlabel(['nz = ',num2str(length(ss))]);
set(gca,'xlim',[0 n+1],'ylim',[0 m+1],'ydir','reverse', ...
   'grid','none','plotboxaspectratio',[n+1 m+1 1]);

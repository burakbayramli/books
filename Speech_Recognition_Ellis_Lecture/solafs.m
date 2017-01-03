function Y = solafs(X, F, W, Wov,  Kmax, Wsim, xdecim, kdecim)
% Y = solafs(X, F, W, Wov, Kmax, Wsim, xdec, kdec)   Do SOLAFS timescale mod'n
%   Y is X scaled to run F x faster.  X is added-in in windows
%   W pts long, overlapping by Wov points with the previous output.  
%   The similarity is calculated over the last Wsim points of output.
%   Maximum similarity skew is Kmax pts.
%   Each xcorr calculation is decimated by xdecim (8)
%   The skew axis sampling is decimated by kdecim (2)
%   Defaults (for 22k) W = 200, Wov = W/2, Kmax = 2*W, Wsim=Wov.
%   Based on "The SOLAFS time-scale modification algorithm", 
%   Don Hejna & Bruce Musicus, BBN, July 1991.
% 1997may16 dpwe@icsi.berkeley.edu $Header: /homes/dpwe/matlab/dpwebox/RCS/solafs.m,v 1.3 2006/04/09 20:10:20 dpwe Exp $
% 2006-04-08: fix to predicted step size, thanks to Andreas Tsiartas

if (nargin < 3)		W    = 200; end
if (nargin < 4)		Wov  = W/2; end
if (nargin < 5)		Kmax = 2 * W; end
if (nargin < 6)         Wsim = Wov; end
if (nargin < 7)         xdecim = 8; end
if (nargin < 8)         kdecim = 2; end

Ss = W - Wov;

if(size(X,1) ~= 1)   error('X must be a single-row vector');  end;

xpts = size(X,2);
ypts = round(xpts / F);
Y = zeros(1, ypts);

% Cross-fade win is Wov pts long - it grows
xfwin = (1:Wov)/(Wov+1);

% Index to add to ypos to get the overlap region
ovix = (1-Wov):0;
% Index for non-overlapping bit
newix = 1:(W-Wov);
% Index for similarity chunks
% decimate the cross-correlation
simix = (1:xdecim:Wsim) - Wsim;

% prepad X for extraction
padX = [zeros(1, Wsim), X, zeros(1,Kmax+W-Wov)];

% Startup - just copy first bit
Y(1:Wsim) = X(1:Wsim);

xabs = 0;
lastxpos = 0;
lastypos = 0;
km = 0;
for ypos = Wsim:Ss:(ypts-W);
  % Ideal X position
  xpos = F * ypos;
%  disp(['xpos=',num2str(xpos),' ypos=',num2str(ypos)]);
  % Overlap prediction - assume all of overlap from last copy
  kmpred = km + ((xpos - lastxpos) - (ypos - lastypos));
  lastxpos = xpos;
  lastypos = xpos;
  if (kmpred <= Kmax && kmpred >= 0) 
    km = kmpred;   % no need to search
  else
    % Calculate the skew, km
    % .. by first figuring the cross-correlation
    ysim = Y(ypos + simix);
    % Clear the Rxy array
    rxy = zeros(1, Kmax+1);
    rxx = zeros(1, Kmax+1);
    % Make sure km doesn't take us backwards
    %Kmin = max(0, xabs-xpos);
    Kmin = 0;
    % actually, this sounds kinda bad.  Allow backwards for now
    for k = Kmin:kdecim:Kmax
      xsim = padX(floor(Wsim + xpos + k + simix));
      rxx(k+1) = norm(xsim);
      rxy(k+1) = (ysim * xsim');
    end
    % Zero the pts where rxx was zero
    Rxy = (rxx ~= 0).*rxy./(rxx+(rxx==0));
    % Local max gives skew
    km = min(find(Rxy == max(Rxy))-1);
  end
  xabs = xpos+km;
%  disp(['ypos = ', int2str(ypos), ', km = ', int2str(km), '(base = ', int2str(ypos-xabs), ')']);
  
%  subplot(311);
%  plot(ysim);
%  subplot(312);
%  plot(padX(Wsim + xpos + ((1-Wsim):Kmax)))
%  subplot(313);
%  plot(Rxy);
%  pause;
  
  % Cross-fade some points
  Y(ypos+ovix) = ((1-xfwin).*Y(ypos+ovix)) + (xfwin.*padX(floor(Wsim+xabs+ovix)));
  % Add in remaining points
  Y(ypos+newix) = padX(floor(Wsim+xabs+newix));
end

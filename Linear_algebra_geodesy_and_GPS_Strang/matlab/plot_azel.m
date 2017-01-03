%*******************************************************
% function hpol = plot_azel(az,el,svs,fs)
%
% DESCRIPTION:
%  Creates an az-el plot of satellites
%  
% ARGUMENTS:
%  az - vector of azimuth angles, in degrees
%  el - vector of elevation angles, in degrees
%  svs - vector of satellite PRN numbers
%    NOTE: To avoid printing PRN numbers on the plot, make 'svs' a vector
%    of zeros.
%  fs (opt) - fontsize for labels
%  
% OUTPUT:
%  hpol - handle to polar plot
%  
% CALLED BY:
%  almanac
%
% FUNCTIONS CALLED:
%  None
%*******************************************************
function hpol = plot_azel(az,el,svs,fs)

line_style = 'auto';

if nargin < 1
	error('Requires 3 input arguments.')
end

if isstr(az) | isstr(el)
	error('Input arguments must be numeric.');
end
if any(size(az) ~= size(el))
	error('AZ and EL must be the same size.');
end

% get hold state
cax = newplot;
next = lower(get(cax,'NextPlot'));
hold_state = ishold;

% get x-axis text color so grid is in same color
tc = get(cax,'xcolor');

% Hold on to current Text defaults, reset them to the
% Axes' font attributes so tick marks use them.
fAngle  = get(cax, 'DefaultTextFontAngle');
fName   = get(cax, 'DefaultTextFontName');
fSize   = get(cax, 'DefaultTextFontSize');
fWeight = get(cax, 'DefaultTextFontWeight');
set(cax, 'DefaultTextFontAngle',  get(cax, 'FontAngle'), ...
	'DefaultTextFontName',   get(cax, 'FontName'), ...
	'DefaultTextFontSize',   get(cax, 'FontSize'), ...
	'DefaultTextFontWeight', get(cax, 'FontWeight') )

if nargin == 3
    fs = fSize;
end

% only do grids if hold is off
if ~hold_state

    % make a radial grid
	hold on;
	hhh=plot([0 2*pi],[0 90],'-','linewidth',0.5);
	v = [get(cax,'xlim') get(cax,'ylim')];
	ticks = length(get(cax,'ytick'));
	delete(hhh);
    
    % check radial limits and ticks
	rmin = 0; rmax = v(4); rticks = ticks-1;
    
	if rticks > 5   % see if we can reduce the number
		if rem(rticks,2) == 0
			rticks = rticks/2;
		elseif rem(rticks,3) == 0
			rticks = rticks/3;
		end
	end

    % define a circle
	th = 0:pi/50:2*pi;
	xunit = cos(th);
	yunit = sin(th);
    
    % now really force points on x/y axes to lie on them exactly
    inds = [1:(length(th)-1)/4:length(th)];
    xunits(inds(2:2:4)) = zeros(2,1);
    yunits(inds(1:2:5)) = zeros(3,1);
    
    % Elevation labels
	rinc = (rmax-rmin)/rticks;
	for i=(rmin+rinc):rinc:rmax
		plot(yunit*i,xunit*i,'-','color',tc,'linewidth',0.5);
		text(0,i+rinc/20,['  ' num2str(90-i)],'verticalalignment','bottom', 'fontweight', 'bold', 'fontsize', fs);
	end

    % plot spokes
	th = (1:6)*2*pi/12;
	cst = cos(th); snt = sin(th);
	cs = [cst; -cst];
	sn = [snt; -snt];
	plot(rmax*sn,rmax*cs,'-','color',tc,'linewidth',0.5);

    % annotate azimuth spokes in degrees
	rt = 1.1*rmax;
	for i = 1:max(size(th))
		text(rt*snt(i),rt*cst(i),int2str(i*30),'horizontalalignment','center', 'fontweight', 'bold', 'fontsize', fs);
		if i == max(size(th))
			continue;
		else
			loc = int2str(180+i*30);
		end
		text(-rt*snt(i),-rt*cst(i),loc,'horizontalalignment','center', 'fontweight', 'bold', 'fontsize', fs);
	end

    % set viewto 2-D
	view(0,90);
    % set axis limits
	axis(rmax*[-1 1 -1.1 1.1]);
end

% Reset defaults.
set(cax, 'DefaultTextFontAngle', fAngle , ...
	'DefaultTextFontName',   fName , ...
	'DefaultTextFontSize',   fSize, ...
	'DefaultTextFontWeight', fWeight );

% set(gcf, 'color', 'white');

% transform data to Cartesian coordinates.
yy = (90-el).*cos(az*pi/180);
xx = (90-el).*sin(az*pi/180);

% plot data on top of grid
q = plot(xx,yy,'*k','MarkerSize',2);

% Place satellite PRN numbers with satellite position 
for i = 1:length(svs)
    if(svs(i)~=0)
        text(xx(i)+3,yy(i),int2str(svs(i)), 'fontweight', 'bold');
    end
end

if nargout > 0
	hpol = q;
end

if ~hold_state
	axis('equal');axis('off');
end

% set hold state
if ~hold_state
    hold on;
end
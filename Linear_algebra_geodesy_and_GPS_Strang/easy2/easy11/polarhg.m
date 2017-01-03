%function hpol = polarhg(theta,rho,p1,v1,p2,v2,p3,v3,p4,v4,p5,v5,p6,v6,p7,v7,p8,v8)
function hpol = polarhg(rtick)

theta = 0;
rho   = 0;

% THIS VERSION OF POLARHG HAS BEEN CHANGED IN LINE 134,135 AND 172
% TO WORK IN MATLAB R13 V 6.5 BY TK TUE@KYNDAL.DK 23-11-02.
%
% POLARHG is similar to polar; however, it is possible to set some
% pseudo-properties.  Below is a table of the pseudo-properties,
% their function, and settings:
%
% PSEUDO-
% PROPERTY    FUNCTION                SETTING/OPTIONS
% --------    --------                ---------------
% theta       The theta values        any valid vector
% rho         The rho values          any valid vector
% tdir        Controls the direction  [ClockWise|{CounterClockWise}]
%             that the angles are
%             labeled.
% rlim        Rho limits               NaN -> 2-element vector [min max]
% rtick       Rho tick mark            NaN -> any valid vector
%             location
% tstep       Step used for            30  -> scalar in degrees
%             Drawing the spokes
% torig       Origin of theta          [Up|Down|Left|{Right}]
% color       Color of trace           [RGB Vector|Colorspec|{'Y'}]
% linestyle   Line style               [{-}|+|--|:|.|x|o|*]
%
% Examples of use:
%
% theta = 0:pi/5:pi;
% rho = 10*rand(size(theta));
% h = polarhg(theta,rho,'tdir','clockwise','rlim',[0 10], ...
%            'rtick',[0 3 6 9],'tstep',45,'torig','down', ...
%            'color','m','linestyle',':');
% SEE ALSO:  POLAR


% Written by John L. Galenski III  11/01/93-04/07/94
% All Rights Reserved
% LDM052694jlg


%%%%%  This M-file has not been tested by the MathWorks, Inc.
%%%%%  There are some known problems with error checking,
%%%%%  however, the M-file is operational.


%%%%%  Note, the grid for the polar plot now consists of two
%%%%%  lines.  One connects the spokes, and the other connects
%%%%%  the circles.


%%%%%  Future enhancements:
%%%%%
%%%%%    ttick         location of spokes


if nargin == 0
    rtick = [0 30 60 90];
else
    rtick = 90 - rtick;
    rtick(find(rtick>=90)) = [];
    rtick(find(rtick<=0))  = [];
    rtick = [0 rtick 90];
end


% default values
color     = 'k';
rlim      = [0 90];
tdir      = 'ClockWise';
tstep     = 30;
torig     = 'up';
linestyle = '.';

%
%
% % Create the property name/property value string arrays
% PropFlag = zeros(1,7);
% for X = 1:(N-2)/2
%   p = eval(['p',int2str(X)]);
%   v = eval(['v',int2str(X)]);
%   if X == 1
%     Property_Names = p;
%     Property_Value = v;
%   else
%     Property_Names = str2mat(Property_Names,p);
%     Property_Value = str2mat(Property_Value,v);
%   end
%   if strcmp(p,'color')
%     PropFlag(1) = 1;
%     color = v;
%   elseif strcmp(p,'rtick')
%     PropFlag(2) = 1;
%     rtick = v;
%   elseif strcmp(p,'rlim')
%     PropFlag(3) = 1;
%     rlim = v;
%   elseif strcmp(p,'tdir')
%     PropFlag(4) = 1;
%     tdir = v;
%   elseif strcmp(p,'tstep')
%     PropFlag(5) = 1;
%     tstep = v;
%   elseif strcmp(p,'torig')
%     PropFlag(6) = 1;
%     torig = v;
%   elseif strcmp(p,'linestyle')
%     PropFlag(7) = 1;
%     linestyle = v;
%   else
%     error(['Invalid pseudo-property name: ',p])
%   end
% end
%
%
% % Determine which properties have not been set by
% % the user
% NotSet = find(PropFlag == 0);
% Default_Settings = ['''y''                   ';
%                     'NaN                   ';
%                     'NaN                   ';
%                     '''counterclockwise''    ';
%                     '30                    ';
%                     '''right''               ';
%                     '''-''                   '];
% Property_Names =   ['color    ';
%                     'rtick    ';
%                     'rlim     ';
%                     'tdir     ';
%                     'tstep    ';
%                     'torig    ';
%                     'linestyle'];
% for I = 1:length(NotSet)
%   eval([Property_Names(NotSet(I),:),'=',Default_Settings(NotSet(I),:),';'])
% end

% Start
CurrentAxes = newplot;
NextPlot = get(CurrentAxes,'NextPlot');
HoldFlag = ishold;
AxisColor = get(CurrentAxes,'XColor');


if ~HoldFlag
    hold on
    % make a radial grid
    if ~isnan(rlim)                   % rlim is defined
        MinRho = find(rho<min(rlim));   % Minimum rho limit
        MaxRho = find(rho>max(rlim));   % Maximum rho limit
        rho([MinRho,MaxRho]) = []; %.*ones(size([MinRho,MaxRho])); TK
        theta([MinRho,MaxRho]) = [];%.*ones(size([MinRho,MaxRho])); TK
    end
    Temp=plot([0 max(theta(:))],[0 max(abs(rho(:)))]); % Initialize plotting info
    AxisLim = [get(CurrentAxes,'xlim') get(CurrentAxes,'ylim')];
    NumTicks = length(get(CurrentAxes,'ytick'));
    delete(Temp);

    % check radial limits and ticks
    if isnan(rtick)                   % rtick not defined
        if ~isnan(rlim)                 % rlim is defined
            Rmin = rlim(1);               % Initialize Rmin
            Rmax = rlim(2);               % Initialize Rmax
        else                            % rlim is not defined
            Rmin = 0;                     % Set Rmin = 0
            Rmax = AxisLim(4);            % Set Rmax = maximum y-axis value
        end
        NumTicks = NumTicks-1;          % Number of circles
        if NumTicks > 5                 % see if we can reduce the number
            if rem(NumTicks,2) == 0
                NumTicks = NumTicks/2;
            elseif rem(NumTicks,3) == 0
                NumTicks = NumTicks/3;
            end
        end
        Rinc = (Rmax-Rmin)/NumTicks;    % Distance between circles
        rtick = (Rmin+Rinc):Rinc:Rmax;  % radii of circles
    else                              % rtick is defined
        if isnan(rlim)                  % rlim is not defined
            Rmin = 0;                     % set Rmin = 0
            Rmax = max(rtick);            % set Rmax = max rtick value
        else                            % rlim is defined
            Rmin = min(rlim);             % set Rmin = minimum rlim
            Rmax = max(rlim);             % set Rmax = maximum rlim
            RtickMin = find(rtick<Rmin);  % find elements of rtick < min(rlim)
            RtickMax = find(rtick>Rmax);  % find elements of rtick > max(rlim)
            % remove these values from rtick
            rtick([RtickMin,RtickMax]) = [];%.*ones(size([RtickMin,RtickMax])); TK
        end
        rtick = [Rmin,rtick,Rmax];      % the new radii
        set(CurrentAxes,'Ylim',[Rmin Rmax]) % set the Y-limits to [Rmin Rmax]
        %set(gca,'YTickLabel',[1;10;100])
        %set(gca,'ZTickLabel',{'90';'60';'30';'0'})
        %set(CurrentAxes,'TickDir','in')
        %set(gca,'ZDir','reverse')
        NumTicks = length(rtick)-1;     % number of circles
    end

    % plot spokes
    th = (1:.5*360/tstep)*2*pi*tstep/360;  % define the spokes
    cst = cos(th);
    snt = sin(th);
    cs = [-cst; cst];
    sn = [-snt; snt];
    cs = [cs;NaN.*cs(1,:)];
    sn = [sn;NaN.*sn(1,:)];
    % plot the spokes
    hh = plot((Rmax-Rmin)*cs(:),(Rmax-Rmin)*sn(:),'-', ...
        'color',AxisColor,'linewidth',1);
    set(0,'UserData',hh)

    % annotate spokes in degrees
    Rt = 1.1*(Rmax-Rmin);
    for i = 1:max(size(th))
        text(Rt*cst(i),Rt*snt(i),int2str(i*tstep),'horizontalalignment','center');
        if i == max(size(th))
            loc = int2str(0);
        else
            loc = int2str(180+i*tstep);
        end
        text(-Rt*cst(i),-Rt*snt(i),loc,'horizontalalignment','center');
    end

    % set view to 2-D.  Use the appropriate view (tdir)
    tdir = lower(tdir);
    torig = lower(torig);
    if strcmp(tdir(1:5),'count') & strcmp(torig,'right')
        view(0,90);
        InitTh = 0;
    elseif strcmp(tdir(1:5),'count') & strcmp(torig,'left')
        view(180,90);
        InitTh = 1;
    elseif strcmp(tdir(1:5),'count') & strcmp(torig,'up')
        view(-90,90);
        InitTh = 2;
    elseif strcmp(tdir(1:5),'count') & strcmp(torig,'down')
        view(90,90);
        InitTh = 3;
    elseif strcmp(tdir(1:5),'clock') & strcmp(torig,'right')
        view(0,-90);
        InitTh = 0;
    elseif strcmp(tdir(1:5),'clock') & strcmp(torig,'left')
        view(180,-90);
        InitTh = 1;
    elseif strcmp(tdir(1:5),'clock') & strcmp(torig,'up')
        view(90,-90);
        InitTh = 2;
    elseif strcmp(tdir(1:5),'clock') & strcmp(torig,'down')
        view(-90,-90);
        InitTh = 3;
    else
        error('Invalid TDir or TOrig')
    end
    if strcmp(torig,'up') | strcmp(torig,'down')
        axis square
    end

    % define a circle
    th = 0:pi/75:2*pi;
    xunit = cos(th);
    yunit = sin(th);

    % This code has been vectorized so that only one line
    % is used to draw the circles.
    MultFact = rtick-Rmin;
    %MultFact = 90-MultFact

    [MM,NN] = size(MultFact);
    [MMM,NNN] = size(xunit);
    XUNIT = [MultFact' * ones(1,NNN)] .* [ones(NN,1) * xunit];
    YUNIT = [MultFact' * ones(1,NNN)] .* [ones(NN,1) * yunit];
    XUNIT = XUNIT';
    YUNIT = YUNIT';
    XX = [XUNIT;NaN.*XUNIT(1,:)];
    YY = [YUNIT;NaN.*YUNIT(1,:)];
    hhh = plot(XX(:),YY(:),'-','Color',AxisColor,'LineWidth',1);
    % Add the text and make sure that it always starts at the origin
    % and moves up.
    for i = MultFact
        if InitTh == 0       % Right
            Xt = 0;
            Yt = i;
        elseif InitTh == 1   % Left
            Xt = 0;
            Yt = -i;
        elseif InitTh == 2   % Up
            Xt = i;
            Yt = 2;
        elseif InitTh == 3   % Down
            Xt = -i;
            Yt = 0;
        else
            Xt = 0;
            Yt = i;
        end
        if (90-i+Rmin) == 0
            Yt = 5;
        end
        text(Xt,Yt,num2str(90-i+Rmin),'VerticalAlignment','bottom', ...
            'HorizontalAlignment','left');
    end

    % set axis limits
    axis((Rmax-Rmin)*[-1 1 -1.1 1.1]);
else
    Rmin = min(rlim);
    Rmax = max(rlim);
end
% transform data to Cartesian coordinates.
xx = (rho-Rmin).*cos(theta); % I'm not sure if this is correct
yy = (rho-Rmin).*sin(theta);

% plot data on top of grid
q = plot(xx,yy,'Color',color,'Marker',linestyle);

if nargout > 0
    hpol = q;
end
if ~HoldFlag
    axis('equal');axis('off');
end

% reset hold state
if ~HoldFlag
    set(CurrentAxes,'NextPlot',NextPlot);
end

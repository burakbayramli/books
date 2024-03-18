%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOWEvO                               %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%                              By:                                  %
%                 Erik Olsson, Created Okt. 19 2005                 %
%                        Based on code by:                          %
%            Alexander von Essen, Created Jan. 18 2005              %
%                                                                   %
%                   Last modified: Okt.  19 2005
%            MG additions: Levmax, Nodes, x, a, vl {1:Levmax}       %
%            saving in file decommissioned
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name:                                                             %
% Purpose:                                                          %
% Called by:                                                        %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function handles = handle_grid_MG(handles)
% xthroat,Athroat,x,a,vol = f(ShockPos,Nodes,InletArea,OutletArea)
levmax = handles.Data.Levmax
xshock = handles.Data.ShockPos;
A1     = handles.Data.InletArea;
A2     = handles.Data.OutletArea;

mode     = 1;
gridtype = 1;
for k = 1:levmax
    nnodes = handles.Data.Nodes{k};
    npnodes= nnodes-2;
    if mode % Should the program generate a new grid?
        switch gridtype
            
            case 1 % Generates a grid with equally spaced x-coordinates.
                xp = linspace(0,1,npnodes)';
                x = [-xp(2); xp; xp(end)+xp(2)];
            otherwise
                error(' only grid type 1 allowed')
                %   case 2 % Generates a grid with shock centered spacing. decommissioned
                %             xp = linspace(0,1,npnodes)';
                %             xpre= xp(1:round(npnodes*xshock));
                %             xaft= xp(round(npnodes*xshock)+1:end);
                %             xp = [xpre.*sin(xpre/max(xpre)*pi/2), xaft.*cos(xaft/max(xaft)*pi/2);
                %             x(2:nppre+1) = xshock.*cos(alpha);
                %             theta = linspace((pi/npnodes),(pi/2),nppost);
                %             x(nppre+2:end-1) = xshock + (1 - xshock).*(1-cos(theta));
                %             x(1) = x(2) - x(3);
                %             x(end) = x(end-1) + x(end-2) - x(end-3);
        end
        a=zeros(size(x));
        
        % Area distribution.
        for n = 2:(nnodes-1)
            if x(n) < 0.35
                a(n) = 1 + 0.5*(A1-1)*(1+cos((pi*x(n))/0.35));
            else
                a(n) = 1 + 0.5*(A2-1)*(1-cos((pi*(x(n)-0.35))/0.65));
            end
        end
        a = [a(2); a(2:end-1); a(end-1)];
        
        % Control volumes.
        da=a(1:end-2)+2*a(2:end-1)+a(3:end);
        vol=(x(3:end)-x(1:end-2)).*da(1:end)/8;
        vol=[vol(1); vol; vol(end)];
        
   % Store x a vol in laval.mat
   % decommissioned for MG implementation
   %    save laval x a vol 
    end
  % If not the previous grid geometry is loaded
  % load laval.mat % Load x, a and vol into memory.
    [Athroat, xthroat]   = min(a);
    handles.Data.Athroat = Athroat;
    handles.Data.xthroat = xthroat;
    handles.Data.x{k}    = x;
    handles.Data.a{k}    = a;
    handles.Data.vol{k}  = vol;
end

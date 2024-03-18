%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Feb. 15 2005             %
%                   Last modified: May 10 2005                      %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: exact_solution.m                                            %
% Purpose: Calculates exact solution.                     %
% Called by: main.m                                                 %
% Calls: am.m                                                       %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [handles] = exact_solution(handles)
disp('exact_sol 19')
% ... = f(ShockPos,gamma,Athroat,p01,R,t01, a,x)
xshock  = handles.Data.ShockPos ;
gamma   = handles.Data.gamma;
Athroat = handles.Data.Athroat ;
p01     = handles.Data.p01;
p2      = handles.Data.p2;
R       = handles.Data.R ;
t01     = handles.Data.t01;
a       = handles.Data.a;
x       = handles.Data.x;
if norm(a-mean(a)*ones(size(a)))> 0.001  
% real nozzle
  solbystate = get(handles.radiobuttonbyState,'value')
  if solbystate
    'blosk'
    [uu,pp,rrho]= newexsol(p01,t01,p2,a'/Athroat);
    'newexsol ok'
    handles.Data.xplot = x;
    handles.Data.pex   = pp;
    handles.Data. p2   = p2;
    handles.Data.rhoexact = rrho;
    handles.Data.M      = uu./sqrt(gamma*pp./rrho);
    handles.Data.Texact = pp./rrho/R;
    handles.Data.spofs  = sqrt(gamma*pp./rrho);
  else
    pre_shock = find(x<=xshock);
    ind  = 0;
    Msub = [];
    Msup = [];
    M    = [];
    xplot = [];
    % Flowfield before shock wave.
    for ix = 1:pre_shock(end)
      xplot = [xplot; x(ix)];
      ind   = ind + 1;
      [M1,M2] = am(a(ind),gamma,Athroat);
      Msub = [Msub M1];
      Msup = [Msup M2];
      if a(ix)<a(ix+1)
        Mnew = M2;
      else
        Mnew = M1;
      end
      M = [M Mnew];
    end
    
    % Across shock wave.
    M1 = M(end);
    M2 = sqrt((1+((gamma-1)/2)*M1^2)/(gamma*M1^2-(gamma-1)/2));
    Astar = a(pre_shock(end))/sqrt(1/M2^2*(2/(gamma+1)*(1+(gamma-1)/2*M2^2))^((gamma+1)/(gamma-1)));
    
    % Flowfield after shock wave.
    if M2 > 1
      disp('Numerical error in exact solution...');
      return
    else
        for ix = pre_shock(end):length(x)
            xplot = [xplot; x(ix)];
            [M1,M2] = am(a(ix),gamma,Astar);
            Msub = [Msub M1];
            Msup = [Msup M2];
            Mnew = M1;
            M = [M Mnew];
        end
    end
    
    % Calculate variables using isentropic flow equations.
    pexact = 1./((1+((gamma-1)/2).*M.^2).^(gamma/(gamma-1)));
    Texact = (1+(gamma-1)/2*M.^2).^(-1);
    rhoexact = 1./(1+(gamma-1)/2*M.^2).^(1/(gamma-1)); 
    pex = zeros(size(pexact));
    pex(1:pre_shock(end)) = pexact(1:pre_shock(end)).*p01;
    Texact(1:pre_shock(end)) = Texact(1:pre_shock(end)).*t01; 
    gam1  = gamma - 1;
    gap1  = gamma + 1;
    M12   = M(pre_shock(end))^2;
    cp    = (gamma*R)/gam1;
    term1 = (1+(2*gamma/gap1)*(M12-1));
    term2 = (2+gam1*M12)/(gap1*M12);
    term3 = term1;
    s2ms1 = cp*log(term1*term2) - R*log(term3);
    p02p01 = exp(-s2ms1/R);
    
    %After Shock
    T1=0.445*Texact(pre_shock(end))*(((2*gamma*M12-(gamma-1))*((gamma-1)*M12+2))/((gamma+1)^2+M12));
    
    %p02 = p01*0.6749;
    p02 = p01*p02p01;
    t01 = t01*p02p01;
    
    pex((pre_shock(end)+1):end) = pexact((pre_shock(end)+1):end).*p02;
    Texact(pre_shock(end)+1:end) = Texact((pre_shock(end)+1):end).*T1;
    
    rhoexact(1:pre_shock(end))=pex(1:pre_shock(end))./(R*Texact(1:pre_shock(end)));
    spofs=sqrt(gamma*R*Texact);
    
    pex=pex';
    p2=pex(end);
    
    rho1=rhoexact(pre_shock(end))*((gamma+1)*M(pre_shock(end))^2)/((gamma-1)*M(pre_shock(end))^2+2);
    %rhotmp=(pex(pre_shock(end)+1:end)./(R*Texact(pre_shock(end)+1:end)'))
    rho02rho01=((((gamma+1)*M12)/((gamma-1)*M12+2))^(gamma/(gamma-1)))*((gamma+1)/(2*gamma*M12-(gamma-1)))^(1/(gamma-1));
    
    rhoexact(pre_shock(end)+1:end)=(rhoexact(pre_shock(end)+1:end)*rho1)'*1.17;%*1./rho02rho01;
    
    % pexact = pexact.*p01;
    % Plot exact solution to gui main axes.
    % guiplot('main',xplot,pexact,'')
    % tplot(xplot,pexact)
    % Calculate exit pressure. (Used as B.C. in numerical solution.
    % p2 = p01*pexact(end);
    handles.Data.xplot = xplot;
    handles.Data.pex   = pex;
    handles.Data. p2   = p2;
    handles.Data.rhoexact = rhoexact';
    handles.Data.M      = M';
    handles.Data.Texact = Texact';
    handles.Data.spofs  = spofs';
  end  
else % shock tube, never mind exact solution
    % well, do
    [xe,p5,rho5,u5,...
          V,p3,rho3,u3,...
          C,p2,rho2,u2] = exactshocktube(handles);
    handles.Data.xplot  = x;
    handles.Data.pex    = p01*ones(size(x));
   % handles.Data. p2   = p2;
    handles.Data.rhoexact = p01/t01/R*ones(size(x));
    handles.Data.M      = 0.001*ones(size(x));
    handles.Data.Texact = t01*ones(size(x));
    handles.Data.spofs  = sqrt(gamma*R*t01*ones(size(x)));
    
    handles.Data.xe   = xe;
    handles.Data.p5   = p5;
    handles.Data.rho5 = rho5;
    handles.Data.u5   = u5;
    
    handles.Data.V    = V;
    handles.Data.p3   = p3;
    handles.Data.rho3 = rho3;
    handles.Data.u3   = u3;
    
    handles.Data.C    = C;
    handles.Data.pp2  = p2;
    handles.Data.rho2 = rho2;
    handles.Data.u2   = u2;
end

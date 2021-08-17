function [rhsHx, rhsHy, rhsEz] = MaxwellPNonConRHS2D(pinfo, Hx,Hy,Ez)

% function [rhsHx, rhsHy, rhsEz] = MaxwellPNonConRHS2D(pinfo, Hx,Hy,Ez)
% Purpose  : Evaluate RHS flux in 2D Maxwell TM form 

Globals2D;

% Initialize storage for right hand side residuals
rhsHx = zeros(size(Hx)); rhsHy = zeros(size(Hy)); rhsEz = zeros(size(Ez));

% For each possible polynomial order
for N=1:length(pinfo)
  
  % Extract information for this polynomial order
  pinf = pinfo(N);
  K = pinf.K;

  % Check to see if any elements of this order exist
  if(K>0)

    % Find location of N'th order nodes
    ids = pinf.ids; Fmask = pinf.Fmask; 
    
    % Extract N'th order nodes
    HxN = Hx(ids); HyN = Hy(ids); EzN = Ez(ids);
    
    % Extract '-' traces of N'th order nodal data
    HxM = HxN(Fmask(:),:); HyM = HyN(Fmask(:),:); EzM = EzN(Fmask(:),:);
    
    % Storage for '+' traces
    HxP = zeros(size(HxM)); HyP = HxP; EzP = HxP;
    
    % For each possible order
    for N2=1:length(pinfo)
      
      % Check to see if any neighbor nodes of this order were located
      if(length(pinf.fmapM{N2}(:))>0)
	
	    % L2 project N2'th order neighbor data onto N'th order trace space
	    interp = pinf.interpP{N2}; fmapM = pinf.fmapM{N2}; vmapP = pinf.vmapP{N2};
	
	    HxP(fmapM) = interp*Hx(vmapP); HyP(fmapM) = interp*Hy(vmapP);
	    EzP(fmapM) = interp*Ez(vmapP);
      end
    end
    
    % Compute jumps of trace data at faces
    dHx = HxM-HxP;  dHy = HyM-HyP;  dEz = EzM-EzP; 

    % Apply PEC boundary condition at wall boundary faces
    dHx(pinf.mapW) = 0; dHy(pinf.mapW) = 0; dEz(pinf.mapW) = 2*EzM(pinf.mapW);
    
    % evaluate jump in incoming characteristic variable
    dR = -pinf.ny.*dHx + pinf.nx.*dHy + dEz ;

    % Compute flux terms
    fluxHx =  pinf.ny.*dR;
    fluxHy = -pinf.nx.*dR;
    fluxEz =          -dR;
    
    % Evaluate local derivatives of fields
    dHxdr = pinf.Dr*HxN; dHxds = pinf.Ds*HxN;
    dHydr = pinf.Dr*HyN; dHyds = pinf.Ds*HyN;
    dEzdr = pinf.Dr*EzN; dEzds = pinf.Ds*EzN;
    
    % Compute physical derivatives of fields
    dHxdy = pinf.ry.*dHxdr + pinf.sy.*dHxds;
    dHydx = pinf.rx.*dHydr + pinf.sx.*dHyds;
    dEzdx = pinf.rx.*dEzdr + pinf.sx.*dEzds;
    dEzdy = pinf.ry.*dEzdr + pinf.sy.*dEzds;
    
    % Compute right hand sides of the PDE's
    rhsHx(ids) = -dEzdy         + pinf.LIFT*(pinf.Fscale.*fluxHx)/2.0;
    rhsHy(ids) =  dEzdx         + pinf.LIFT*(pinf.Fscale.*fluxHy)/2.0;
    rhsEz(ids) =  dHydx - dHxdy + pinf.LIFT*(pinf.Fscale.*fluxEz)/2.0;
  end 
end
return
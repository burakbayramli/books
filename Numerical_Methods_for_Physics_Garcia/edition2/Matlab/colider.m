function [v,crmax,selxtra,col] = ...
                     colider(v,crmax,tau,selxtra,coeff,sD)
% colide - Function to process collisions in cells
% [v,crmax,selxtra,col] = colider(v,crmax,tau,selxtra,coeff,sD)
% Inputs
%    v         Velocities of the particles
%    crmax     Estimated maximum relative speed in a cell
%    tau       Time step
%    selxtra   Extra selections carried over from last timestep
%    coeff     Coefficient in computing number of selected pairs
%    sD        Structure containing sorting lists 
% Outputs
%    v         Updated velocities of the particles
%    crmax     Updated maximum relative speed
%    selxtra   Extra selections carried over to next timestep
%    col       Total number of collisions processed

ncell = sD.ncell;
col = 0;          % Count number of collisions

%* Loop over cells, processing collisions in each cell
for jcell=1:ncell
	
 %* Skip cells with only one particle
 number = sD.cell_n(jcell);
 if( number > 1 )  
	 
  %* Determine number of candidate collision pairs 
  %  to be selected in this cell
  select = coeff*number^2*crmax(jcell) + selxtra(jcell);
  nsel = floor(select);          % Number of pairs to be selected
  selxtra(jcell) = select-nsel;  % Carry over any left-over fraction
  crm = crmax(jcell);            % Current maximum relative speed
  
  %* Loop over total number of candidate collision pairs
  for isel=1:nsel
    
	%* Pick two particles at random out of this cell
    k = floor(rand(1)*number);
    kk = rem(ceil(k+rand(1)*(number-1)),number);
    ip1 = sD.Xref(k+sD.index(jcell));      % First particle
    ip2 = sD.Xref(kk+sD.index(jcell));     % Second particle
	
	%* Calculate pair's relative speed
    cr = norm( v(ip1,:)-v(ip2,:) );  % Relative speed 
    if( cr > crm )         % If relative speed larger than crm,
      crm = cr;            % then reset crm to larger value
    end

    %* Accept or reject candidate pair according to relative speed
    if( cr/crmax(jcell) > rand(1) )    
	  %* If pair accepted, select post-collision velocities
      col = col+1;                     % Collision counter
      vcm = 0.5*(v(ip1,:) + v(ip2,:)); % Center of mass velocity
      cos_th = 1 - 2*rand(1);          % Cosine and sine of 
      sin_th = sqrt(1 - cos_th^2);     % collision angle theta
      phi = 2*pi*rand(1);              % Collision angle phi
      vrel(1) = cr*cos_th;             % Compute post-collision 
      vrel(2) = cr*sin_th*cos(phi);    % relative velocity
      vrel(3) = cr*sin_th*sin(phi);
      v(ip1,:) = vcm + 0.5*vrel;       % Update post-collision
      v(ip2,:) = vcm - 0.5*vrel;       % velocities
    end

  end % Loop over pairs
  crmax(jcell) = crm;     % Update max relative speed 
 end
end	  % Loop over cells     
return;

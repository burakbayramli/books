function sampD = sampler(x,v,npart,L,sampD)
% sampler - Function to sample density, velocity and temperature
% Inputs
%    x       Particle positions
%    v       Particle velocities
%    npart   Number of particles
%    L       System size
%    sampD   Structure with sampling data
% Outputs
%    sampD   Structure with sampling data

%* Compute cell location for each particle
ncell = sampD.ncell;
jx=ceil(ncell*x/L);

%* Initialize running sums of number, velocity and v^2
sum_n = zeros(ncell,1);
sum_v = zeros(ncell,3);
sum_v2 = zeros(ncell,1);

%* For each particle, accumulate running sums for its cell
for ipart=1:npart
  jcell = jx(ipart);  % Particle ipart is in cell jcell
  sum_n(jcell) = sum_n(jcell)+1;
  sum_v(jcell,:) = sum_v(jcell,:) + v(ipart,:);
  sum_v2(jcell) = sum_v2(jcell) + ...
              v(ipart,1)^2 + v(ipart,2)^2 + v(ipart,3)^2;
end

%* Use current sums to update sample number, velocity 
%  and temperature	  
for i=1:3
  sum_v(:,i) = sum_v(:,i)./sum_n(:);
end
sum_v2 = sum_v2./sum_n;
sampD.ave_n = sampD.ave_n + sum_n;
sampD.ave_u = sampD.ave_u + sum_v;
sampD.ave_T = sampD.ave_T + sum_v2 - ...
         (sum_v(:,1).^2 + sum_v(:,2).^2 + sum_v(:,3).^2);
sampD.nsamp = sampD.nsamp + 1;
return;

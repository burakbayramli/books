function sD = sorter(x,L,sD)
% sorter - Function to sort particles into cells
% sD = sorter(x,L,sD)
% Inputs
%    x       Positions of particles
%    L       System size
%    sD      Structure containing sorting lists
% Output
%    sD      Structure containing sorting lists

%* Find the cell address for each particle
npart = sD.npart;
ncell = sD.ncell;
jx = floor(x*ncell/L) + 1;
jx = min( jx, ncell*ones(npart,1) );

%* Count the number of particles in each cell
sD.cell_n = zeros(ncell,1);
for ipart=1:npart
  sD.cell_n( jx(ipart) ) = sD.cell_n( jx(ipart) ) + 1;
end

%* Build index list as cumulative sum of the 
%  number of particles in each cell
m=1;
for jcell=1:ncell
  sD.index(jcell) = m;
  m = m + sD.cell_n(jcell);
end

%* Build cross-reference list
temp = zeros(ncell,1);     % Temporary array
for ipart=1:npart
  jcell = jx(ipart);       % Cell address of ipart
  k = sD.index(jcell) + temp(jcell);
  sD.Xref(k) = ipart;
  temp(jcell) = temp(jcell) + 1;
end

return;

function [Nv, VX, K, EToV] = MeshReader1DGambit(FileName)

% function [Nv, VX, K, EToV] = MeshReader1DGambit(FileName)
% Purpose  : Read in basic grid information to build grid
%
% NOTE     : gambit(Fluent, Inc) *.neu format is assumed

% Open file and read intro
Fid = fopen(FileName, 'rt');
for i=1:6; line = fgetl(Fid); end

% Find number of nodes and number of elements
dims = fscanf(Fid, '%d');
Nv = dims(1); K = dims(2);
for i=1:2; line = fgetl(Fid); end

% read node coordinates
VX = (1:Nv);
for i = 1:Nv
  line = fgetl(Fid);
  tmpx = sscanf(line, '%lf');
  VX(i) = tmpx(2);
end
for i=1:2; line = fgetl(Fid); end

% read element to node connectivity
EToV = zeros(K, 2);
for k = 1:K
  line   = fgetl(Fid);
  tmpcon = sscanf(line, '%lf');
  EToV(k,1) = tmpcon(4); EToV(k,2) = tmpcon(5);
end

% Close file
st = fclose(Fid);
return

function [Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(FileName)

% function [Nv, VX, VY, K, EToV, BCType] = MeshReaderGambitBC2D(FileName)
% Purpose  : Read in basic grid information to build grid
% NOTE     : gambit(Fluent, Inc) *.neu format is assumed

Globals2D;

Fid = fopen(FileName, 'rt');

% read intro
for i=1:6
  line = fgetl(Fid);
end

% Find number of nodes and number of elements
dims = fscanf(Fid, '%d');
Nv = dims(1); K = dims(2);

for i=1:2
  line = fgetl(Fid);
end

% read node coordinates
VX = (1:Nv); VY = (1:Nv);
for i = 1:Nv
  line = fgetl(Fid);
  tmpx = sscanf(line, '%lf');
  VX(i) = tmpx(2); VY(i) = tmpx(3);
end

for i=1:2
  line = fgetl(Fid);
end

% read element to node connectivity
EToV = zeros(K, 3);
for k = 1:K
  line   = fgetl(Fid);
  tmpcon = sscanf(line, '%lf');
  EToV(k,:) = tmpcon(4:6); 
end

% skip through material property section
for i=1:4
  line = fgetl(Fid);
end;

while isempty(intersect(line,'ENDOFSECTION'))
 line   = fgetl(Fid);
end

line = fgetl(Fid); line = fgetl(Fid);

% boundary codes (defined in Globals2D)
BCType = zeros(K,3);

% Read all the boundary conditions at the nodes
while  line ~= -1
  if ~isempty(strfind(line, 'In')),   bcflag = In; end;
  if ~isempty(strfind(line, 'Out')),  bcflag = Out; end;
  if ~isempty(strfind(line, 'Wall')), bcflag = Wall; end;
  if ~isempty(strfind(line, 'Far')),  bcflag = Far; end;
  if ~isempty(strfind(line, 'Cyl')),  bcflag = Cyl; end;
  if ~isempty(strfind(line, 'Dirichlet')), bcflag = Dirichlet; end;
  if ~isempty(strfind(line, 'Neuman')),    bcflag = Neuman;    end;
  if ~isempty(strfind(line, 'Slip')),    bcflag = Slip;    end;
  
  line   = fgetl(Fid);
  while isempty(strfind(line, 'ENDOFSECTION'))
    tmpid = sscanf(line, '%d');
    BCType(tmpid(1),tmpid(3)) = bcflag;
    line = fgetl(Fid);
  end;
  line = fgetl(Fid); line = fgetl(Fid);
end

% Close file
st = fclose(Fid);
return

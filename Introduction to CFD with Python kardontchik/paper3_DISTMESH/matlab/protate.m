function p=protate(p,phi)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

A=[cos(phi),-sin(phi);sin(phi),cos(phi)];
p=p*A;


% Calculate distance between Nn and referece points Pi
% berechnet Abstände zwischen Nn und die Referenzpunkte Pi
% P= [P1 P2 ...]  ; measured distances : S=[s1 s2 ...]
% Sn = []: calculated distances 
% F : Error norm
function [Sn , F] = distanzen(Nn,P,S)
global P S

for i1=1:length(S)
    Sn(i1)=norm(P(:,i1)-Nn);
end
F = norm(S-Sn);

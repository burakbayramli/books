function e=nllsrho_minz(lvec,infoz,stat,Gn,Gn2)
%An M File of moment conditions
e     =abs(Gn2-Gn*[lvec(1);lvec(1)^2;lvec(2)]);

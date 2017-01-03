function e=nllsrho_minz2(rhovec,infoz,stat,Gn,Gn2)
%An M File to Calculate Spatial error parameter 
e     =abs(Gn2-Gn*[rhovec(1);rhovec(2);rhovec(1)*rhovec(2);rhovec(1)^2;rhovec(2)^2;rhovec(3)]);
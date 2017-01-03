% Detection of 1ms clock resets of Ashtech receiver

%Kai Borre 03-31-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/2  $

dd = datam(1:90,:);   % file of one-way code and phase, master data
f1 = 154;
f2 = 120;
c = 299792458;
alpha1 = f1^2/(f1^2-f2^2);
alpha2 = -f2^2/(f1^2-f2^2);
PS = alpha1*dd(1:90,3)+alpha2*dd(1:90,5);
PHI = alpha1*dd(1:90,4)+alpha2*dd(1:90,6);
for i=1:90
   % test(i) = datam(i,3)-datam(i,5) -(datam(1,3)-datam(1,5))
   % reset(i) = round(test(i)/(c*10.e-3))
   test1(i) = PS(i)-PHI(i) -(PS(1)-PHI(1));
   reset1(i) = round(test1(i)/(c*10.e-3));
end
%%%%%%%%%%%%%%% end outlier.m  %%%%%%%%%%%%%%%%%
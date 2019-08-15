function test01
% Test of elementary rotation matrices

clc, clf
% Select following angles and flag:
ALF = pi/6; BET = pi/6; GAM = pi/6;
flag = 3;
if flag == 1
   X1 = [2;1;0];
   [D1ALF,D2BET,D3GAM] = drehmatrix_el(ALF,BET,GAM);
   Y1 = D3GAM*X1;
   XN1 = [zeros(3,1),X1];
   YN1 = [zeros(3,1),Y1];
   plot(XN1(1,:),XN1(2,:),'k'), hold on
   plot(YN1(1,:),YN1(2,:),'r'), hold on
end
if flag == 2
   X1 = [2;0;1];
   [D1ALF,D2BET,D3GAM] = drehmatrix_el(ALF,BET,GAM);
   Y1 = D2BET*X1;
   XN1 = [zeros(3,1),X1];
   YN1 = [zeros(3,1),Y1];
   plot(XN1(1,:),XN1(3,:),'k'), hold on
   plot(YN1(1,:),YN1(3,:),'g'), hold on
end
if flag == 3
   X1 = [0;2;1];
   [D1ALF,D2BET,D3GAM] = drehmatrix_el(ALF,BET,GAM);
   Y1 = D1ALF*X1;
   XN1 = [zeros(3,1),X1];
   YN1 = [zeros(3,1),Y1];
   plot(XN1(2,:),XN1(3,:),'k'), hold on
   plot(YN1(2,:),YN1(3,:),'b'), hold on
end

axis equal
grid on

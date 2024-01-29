clear all;

% next 3 lines: Trapezoid rule
x=[0 1];
w=[0.5 0.5];
It=sum((x.^3).*w)

% next 3 lines: Trapezoid rule
x=[0 0.5 1];
w=[1/6 4/6 1/6];
Is=sum((x.^3).*w)

Ie=0.25  % exact result

fprintf('Error in Trapezoid rule= %.3f\n',Ie-It);
fprintf('Error in Simposon''s rule= %.3f\n',Ie-Is);
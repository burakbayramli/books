function test_layout()
% TEST_LAYOUT       Demo-function to test some bayesian net layouts
%

% Change History :
% Date		Time		Prog	Note
% 13-Apr-2000	10:40 PM	ATC	Created under MATLAB 5.3.1.29215a (R11.1)

% ATC = Ali Taylan Cemgil,
% SNN - University of Nijmegen, Department of Medical Physics and Biophysics
% e-mail : cemgil@mbfys.kun.nl 

% Make the following network (from Jensen (1996) p84 fig 4.17)
%    1
%  / | \
% 2  3  4
% |  |  |
% 5  6  7
%  \/ \/
%  8   9
% where all arcs point downwards

disp('plot directed')
clf;

N = 9;
dag = zeros(N,N);
dag(1,2)=1; dag(1,3)=1; dag(1,4)=1;
dag(2,5)=1; dag(3,6)=1; dag(4,7)=1;
dag(5,8)=1; dag(6,8)=1; dag(6,9)=1; dag(7,9) = 1;

draw_layout(dag);

pause
clf
disp('plot undirected')
udag = [dag+dag'];
draw_layout(udag);

pause
clf
disp('plot mixed')
mg = [dag];
mg(2,1) = 1; mg(8,5) = 1;
draw_layout(mg);

pause

clf
sz = get(0, 'ScreenSize');
sv = get(gcf, 'pos');
set(gcf, 'units','pix','pos', sz);
disp('plot BAT intra')
names = {'LeftClr', 'RightClr', 'LatAct', 'Xdot', 'InLane', 'FwdAct', ...
      'Ydot', 'Stopped', 'EngStatus', 'FBStatus', ...
      'LeftClrSens', 'RightClrSens', 'TurnSignalSens', 'XdotSens', 'YdotSens', ...
      'FYdotDiffSens', 'FclrSens', 'BXdotSens', 'BclrSens', 'BYdotDiffSens', ...
      'SensorValid', 'FYdotDiff', 'FcloseSlow', 'Fclr', 'BXdot', 'BcloseFast', 'Bclr', 'BYdotDiff'};

intrac = {...
      'LeftClr', 'LeftClrSens';
  'RightClr', 'RightClrSens';
  'LatAct', 'TurnSignalSens'; 'LatAct', 'Xdot';
  'Xdot', 'XdotSens';
  'FwdAct', 'Ydot';
  'Ydot', 'YdotSens'; 'Ydot', 'Stopped';
  'EngStatus', 'Ydot'; 'EngStatus', 'FYdotDiff'; 'EngStatus', 'Fclr'; 'EngStatus', 'BXdot';
  'SensorValid', 'XdotSens';   'SensorValid', 'YdotSens';
  'FYdotDiff', 'FYdotDiffSens'; 'FYdotDiff', 'FcloseSlow';
  'FcloseSlow', 'FBStatus';
  'Fclr', 'FclrSens'; 'Fclr', 'FcloseSlow';
  'BXdot', 'BXdotSens';
  'Bclr', 'BclrSens'; 'Bclr', 'BXdot'; 'Bclr', 'BcloseFast';
  'BcloseFast', 'FBStatus';
  'BYdotDiff', 'BYdotDiffSens'; 'BYdotDiff', 'BcloseFast'};
[intra, names] = mk_adj_mat(intrac, names, 1);
N = size(intra,1);
isbox = rand(N,1) > 0.5;
draw_layout(intra, names, isbox);

pause
set(gcf, 'pos',sv);
cla
textoval(0.5,0.5,'The End..')
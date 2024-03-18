function vargargout = DemoFlowjo(varargin)
close all
clear all
format compact
global ButWH
%figure(1)
editbleft = 15;
ButWH = 8;
dsgn = 0
if dsgn
  hh = imread('guilook.png');
  image(hh)
  hold on
end
solparams=...
[ 0 720
530 720
530 256
  0 256];
gui.solparams = solparams;
eroeradbut = pt2rect([ 28 666]);
gui.radiobuttonExplicitRoe = eroeradbut;
iroeradbut = pt2rect([304 666]);
gui.radiobuttonImplicitRoe = iroeradbut;
ejamradbut = pt2rect([ 24 698]);
gui.radiobuttonExplicitJameson = ejamradbut;
ijamradbut = pt2rect([304 698]);
gui.radiobuttonImplicitJameson = ijamradbut;

implparams = ...
[ 4 652
506 652
506 513
  4 513];
gui.implparams = implparams;
edith = 632 - 610;
editw = 483 - 13;

itimestepedit =...
 [editbleft       559
 editbleft+editw  559
 editbleft+editw  559-edith
 editbleft        559-edith];
 gui.editTimestepsImplicit = itimestepedit;
 
ivis2edit=...
 [editbleft      596
 editbleft+editw 596
 editbleft+editw 596-edith
 editbleft       596-edith];
 gui.editVis2Implicit = ivis2edit;
 
ivis4edit=...
 [editbleft      631
 editbleft+editw 631
 editbleft+editw 631-edith
 editbleft       631-edith];
 gui.ivis4edit = editVis4Implicit;
 
if dsgn
  tmp = {solparams implparams itimestepedit ivis2edit ivis4edit};
  plquad(tmp)
  tmp = {eroeradbut,iroeradbut,ejamradbut,ijamradbut};
  plquad(tmp)
end

explparams = ...
[4 490
506 492
506 283
4 283];
gui.explparams = explparams;

etimestepedit =...
 [editbleft      328
 editbleft+editw 328
 editbleft+editw 328-edith
 editbleft       328-edith ];
 gui.editTimestepsExplicit = etimestepedit ;
 
evis2edit=...
 [editbleft       364
  editbleft+editw 364
  editbleft+editw 364-edith
  editbleft       364-edith];
  gui.editVis2Explicit = evis2edit;

evis4edit=...
 [editbleft       400
  editbleft+editw 400
  editbleft+editw 400-edith
  editbleft       400-edith];
  gui.editVis4Explicit=evis4edit;
  
ecfledit=...
 [editbleft      436
 editbleft+editw 436
 editbleft+editw 436-edith
 editbleft       436-edith];
 gui.editCFLNumber=ecfledit;
 
eplotintedit=...
 [editbleft 472
 editbleft+editw 472
 editbleft+editw 472-edith
 editbleft       472-edith];
gui.editPlotInterval=eplotintedit;

if dsgn
  tmp = {explparams eplotintedit ecfledit evis4edit evis2edit etimestepedit};
  plquad(tmp)
end
configparams = ...
[ 4 229
531 229
531 -75
  4 -75];
gui.configparams=configparams;

  shkposedit = ...
  [editbleft      36
  editbleft+editw 36
  editbleft+editw 36-edith
  editbleft       36-edith];
gui.editShockPos =   shkposedit;

  p01edit = ...
  [editbleft      74
  editbleft+editw 74
  editbleft+editw 74-edith
  editbleft       74-edith];
gui.editp01=  p01edit;

  t01edit = ...
  [editbleft      110
  editbleft+editw 110
  editbleft+editw 110-edith
  editbleft       110-edith];
  gui.editt01 = t01edit ;
  
  p2edit = ...
  [editbleft      146
  editbleft+editw 146
  editbleft+editw 146-edith
  editbleft       146-edith];
  gui.editp2 = p2edit;
  
shkinputradbut = pt2rect([24 176]);
gui.radiobuttonbyShock= shkinputradbut;
stainputradbut = pt2rect([24 201]);
gui.radiobuttonbyState =stainputradbut;
if dsgn
  tmp = { shkposedit p01edit t01edit p2edit shkinputradbut stainputradbut};
  plquad(tmp)
end

axes2 = ...
[594 520
1240 520
1240 260
 594 260];
gui.axes2 = axes2;
ax2butgroup = ...
[570 240
1240 240
1240 200
570  200];
gui.ax2butgroup = ax2butgroup;

residbut = pt2rect([612 561]);
gui.radiobuttonResidual = residbut;
areabut  = pt2rect([830 559]);
gui.radiobuttonAreadist= areabut;
stepsizebut = pt2rect([1053 560]);
gui.radiobuttonStepsizedist = stepsizebut;

axes1 = ...
[ 594 185
1240  185
1240  -75
 594  -75];
gui.axes1 = axes1;
ax1butgroup = ...
[570 578
1240 578
1240 540
570  540];
gui.ax1butgroup = ax1butgroup;
pressbut = pt2rect([ 600 219]);
gui.radiobuttonPressuredist= pressbut;
velbut   = pt2rect([ 820 219]);
gui.radiobuttonVelocitydist = velbut;
densbut  = pt2rect( [1040 219]);;
gui.radiobuttonDensitydist = densbut;
if dsgn
  tmp = {axes1 axes2 configparams};
  plquad(tmp)
  tmp = {residbut areabut stepsizebut pressbut velbut densbut};
  plquad(tmp)
end

butleft = 560; 
buth    = 40; 
butw    = 720-butleft;

solvebut = ...
[butleft     630
butleft+butw 630
butleft+butw 630-buth
butleft      630-buth];
gui.solveButton= solvebut;

resetbut = ...
[butleft     680
butleft+butw 680
butleft+butw 680-buth
butleft      680-buth];
gui.resetButton = resetbut;

exitbut = ...
[butleft     730
butleft+butw 730
butleft+butw 730-buth
butleft      730-buth];
gui.exitButton= exitbut;
if dsgn
  tmp = {exitbut resetbut solvebut};
  plquad(tmp);
end

infopanel = ...
[920 720
1240 720
1240 590
920  590];
gui.infopanel = infopanel;

txtl = 930;
txtr = 1230;
txth = 18;
dztxt = 22;
z0    = 627;

nittxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.Iterations = nittxt;

elaptimtxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.Timeelapsed = elaptimtxt;

tperittxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.Timeperit = tperittxt;

restxt = ...
[txtl z0
txtr  z0
txtr  z0-txth
txtl  z0-txth];
z0 = z0+dztxt;
gui.Residual = restxt;

timstptxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl  z0-txth];
z0 = z0+dztxt;
gui.Timestep = timstptxt;
if dsgn
  tmp = {infopanel nittxt timstptxt restxt tperittxt elaptimtxt};
  plquad(tmp)
end

uipan = ...
[ -5 740
1270 740
1270 -85
  -5 -85];
gui.uipan = uipan;
if dsgn
  tmp = {uipan};
  plquad(tmp)
end

orig =[-5 740];
upco = [1270 -85];
scxy = 1./(upco-orig)
% xn = (xpix-orig(1))*scxy(1); 
% yn = (ypix-orig(2))*scxy(2)
% xyn = (xypix -orig)*diag(scxy);
cn = fieldnames(gui);
nn = length(cn)
dd = diag(scxy);
if dsgn
  figure(2)
  hold on
end
for k = 1:nn
  str = cn{k};
  xypix = gui.(str);
  xyn  = (xypix - ones(size(xypix,1),1)*orig)*dd;
% xypix = xyn*inv(dd) + ones(size(xyn,1)*orig
  gui.(str) = rect2wh(xyn);
  if dsgn
    plquad({xyn})
    hold on
    xyc = mean(xyn,1);
    text(xyc(1),xyc(2),str);
  end
end
  
  
f = figure('position',[20 50 1200 700]);

edithght = 20
%ax = axes('position',[0.3 0.3 0.5 0.5]);
p   = uipanel(f,"title","DemoFlow", "position", gui.uipan);

ax1 = axes(p,'position',gui.axes1);
plot(0:10,0:10,'k')
x0 = 10
h = 15
w = 120
dx = 135
z0 = 5
ax1butgroup = uibuttongroup(p,'position',gui.ax1butgroup);
pressbuttxt = uicontrol(ax1butgroup,'style','text','string','Pressure','position',[x0 z0 w h])
pressbut = uicontrol(ax1butgroup,'style','radiobutton','position',                [x0+w+5 z0 8 8 ]);
x0 = x0+dx;
velbuttxt = uicontrol(ax1butgroup,'style','text','string','Velocity','position',  [x0 z0 w h]);
velbut    = uicontrol(ax1butgroup,'style','radiobutton','position',               [x0+w+5 z0 8 8 ]);
x0=x0+dx;
densbuttxt = uicontrol(ax1butgroup,'style','text','string','Density','position',  [x0 z0 w h]);
densbut    = uicontrol(ax1butgroup,'style','radiobutton','position',              [x0+w+5 z0 8 8 ]);


ax2 = axes(p,'position',gui.axes2);
ax2butgroup = uibuttongroup(p,'position',gui.ax2butgroup);
x0 = 10
residbuttxt = uicontrol(ax2butgroup,'style','text','string','Residual','position',[x0 z0 w h])
residbut    = uicontrol(ax2butgroup,'style','radiobutton','position',             [x0+w+5 z0 8 8 ]);
x0 = x0+dx;
areabuttxt = uicontrol(ax2butgroup,'style','text','string','Area','position',[x0 z0 w h]);
areabut    = uicontrol(ax2butgroup,'style','radiobutton','position',         [x0+w+5 z0 8 8 ]);
x0=x0+dx;
stepsizebuttxt = uicontrol(ax2butgroup,'style','text','string','Stepsize','position',[x0 z0 w h]);
stepsizebut    = uicontrol(ax2butgroup,'style','radiobutton','position',            [x0+w+5 z0 8 8 ]);

x0 = 10;
w  = 170;
h  = 15;
dz = h + 5;
z0 = 10;
configparams = uipanel(p,'title','Configuration','position',gui.configparams)
cfbutgroup   = uibuttongroup(configparams,'title','Solution def.','position',[10 10 400 20]);

shkinputtxt    = uicontrol(cfbutgroup,'style','text','string','Shock pos.','position',[x0     z0 80 h])
shkinputradbut = uicontrol(cfbutgroup,'style','radiobutton','position',               [x0+80  z0   h h])
x0 = x0+100;
stainputtxt    = uicontrol(cfbutgroup,'style','text','string','States ','position',   [x0    z0 80 h])
stainputradbut = uicontrol(cfbutgroup,'style','radiobutton','position',               [x0+80 z0   h h])
x0 = 10
z0 = 50
shkpostxt  = uicontrol(configparams,'style','text','string','Shock pos.','position',[x0 z0 w h])
shkposedit = uicontrol(configparams,'style','edit','string','0','position',         [x0+w+5 z0 w/2 h])
z0 = z0+dz;
p01txt  = uicontrol(configparams,'style','text','string','P01 ','position',         [x0 z0 w h])
p01edit = uicontrol(configparams,'style','edit','string','0','position',            [x0+w+5 z0 w/2 h])
z0 = z0+dz;
t01txt  = uicontrol(configparams,'style','text','string','T01','position',[x0 z0 w h])
t01edit = uicontrol(configparams,'style','edit','string','0','position',[x0+w+5 z0 w/2 h])
z0=z0+dz;
p2txt  = uicontrol(configparams,'style','text','string','P2 ','position',[x0 z0 w h])
p2edit = uicontrol(configparams,'style','edit','string','0','position',[x0+w+5 z0 w/2 h])

solparams   = uipanel(p,'title','Solver','position',gui.solparams);
solbutgroup = uibuttongroup(solparams,'title','Methods','position',[0.05 0.02 0.8 0.20]);
txtw = 170
eroeradbuttxt = uicontrol(solbutgroup,'style','text','string','Expl. Roe','position',[ 10      10  txtw h]);
eroeradbut    = uicontrol(solbutgroup,'style','radiobutton','position',              [ 10+txtw 10  10   h]); 
iroeradbuttxt = uicontrol(solbutgroup,'style','text','string','Impl. Roe','position',[ 10      30  txtw h]);
iroeradbut    = uicontrol(solbutgroup,'style','radiobutton','position',              [ 10+txtw 30  10   h]);

ejamradbuttxt = uicontrol(solbutgroup,'style','text','string','Expl. Jameson','position',[ 30+txtw      10  txtw h]);
ejamradbut    = uicontrol(solbutgroup,'style','radiobutton','position',                  [ 30+txtw+txtw 10  10   h]);
ijamradbuttxt = uicontrol(solbutgroup,'style','text','string','Impl. Jameson','position',[ 30+txtw      30  txtw h]);
ijamradbut    = uicontrol(solbutgroup,'style','radiobutton','position',                  [ 30+txtw+txtw 30  10   h]);

implparams    = uipanel(solparams,'title','Implicit','position',posrel(gui.implparams,gui.solparams));

z0 = 20;
itimestepedittxt = uicontrol(implparams,'style','text','string','# time steps: ','position',[x0 z0 w h]);
itimestepedit    = uicontrol(implparams,'style','edit','string', num2str(0),'position',[x0+w z0 w/2 h]);
z0 = z0 + dz;
ivis2edittxt = uicontrol(implparams,'style','text','string','Vis2: ','position',[x0 z0 w h]);
ivis2edit    = uicontrol(implparams,'style','edit','string',num2str(0),'position',[x0+w z0 w/2 h]);
z0 = z0 + dz;
ivis4edittxt = uicontrol(implparams,'style','text','string','Vis4: ','position',[x0 z0 w h]);
ivis4edit    = uicontrol(implparams,'style','edit','string',num2str(0),'position',[x0+w z0 w/2 h]);

explparams    = uipanel(solparams,'title','Explicit','position',posrel(gui.explparams,gui.solparams));
z0 = 30;
etimestepedittxt = uicontrol(explparams,'style','text','string','# time steps: ','position',[x0 z0 w h]);
etimestepedit    = uicontrol(explparams,'style','edit','string',num2str(0),'position',[x0+w z0 w/2 h]);
z0 = z0+dz;
ecfledittxt = uicontrol(explparams,'style','text','string','CFL: ','position',[x0 z0 w h]);
ecfledit    = uicontrol(explparams,'style','edit','string',num2str(0),'position',[x0+w z0 w/2 h]);
z0 = z0 + dz
evis2edittxt = uicontrol(explparams,'style','text','string','Vis2: ','position',[x0 z0 w h]);
evis2edit    = uicontrol(explparams,'style','edit','string',num2str(0),'position',[x0+w z0 w/2 h]);
z0 = z0 + dz;
evis4edittxt = uicontrol(explparams,'style','text','string','Vis4: ','position',[x0 z0 w h]);
evis4edit    = uicontrol(explparams,'style','edit','string',num2str(0),'position',[x0+w z0 w/2 h]);
z0 = z0 + dz;
eplotintedittxt = uicontrol(explparams,'style','text','string','Plotinterval: ','position',[x0 z0 w h]);
eplotintedit    = uicontrol(explparams,'style','edit','string', num2str(0),'position',[x0+w z0 w/2 h]);

%tmp = {nittxt timstptxt restxt tperittxt elaptimtxt};
infopanel    = uipanel(p,'title','Info.','position',gui.infopanel);
xl = 10;
w  = 150
h  = 15;
z0 = 10
dz = h + 5;
nittxttxt     = uicontrol(infopanel,'style','text','string','# iterations: ','position', [xl z0 w h]);
nittxt        = uicontrol(infopanel,'style','text','string', num2str(0),'position', [xl+w z0 w/2 h]);
z0 = z0 + dz;
timstptxttxt  = uicontrol(infopanel,'style','text','string','# time steps: ','position',[xl z0 w h]);
timstptxt     = uicontrol(infopanel,'style','text','string', num2str(0),'position',[xl+w z0 w/2 h]);
z0 = z0 + dz;
restxttxt     = uicontrol(infopanel,'style','text','string','Residual: ','position',[xl z0 w h]);
restxt        = uicontrol(infopanel,'style','text','string',num2str(0),'position',[xl+w z0 w/2 h]);
z0 = z0 + dz;
tperittxttxt  = uicontrol(infopanel,'style','text','string','Time per it.: ','position',[xl z0 w h]);
tperittxt     = uicontrol(infopanel,'style','text','string', num2str(0),'position',[xl+w z0 w/2 h]);
z0 = z0 + dz;
elaptimtxttxt = uicontrol(infopanel,'style','text','string','Elapsed time:','position',[xl z0 w h]);
elaptimtxt    = uicontrol(infopanel,'style','text','string',num2str(0),'position',[xl+w z0 w/2 h]);

%tmp = {exitbut resetbut solvebut};
x0 = 600;
z0 = 10
w = 100
h = 30
resetbut = uicontrol(p,'style','pushbutton','string','Reset','position',[x0 z0 w h]);
z0 = z0 + h +5;
exitbut  = uicontrol(p,'style','pushbutton','string','Quit','position',[x0 z0 w h]);
z0 = z0 + h + 5;
solvebut = uicontrol(p,'style','pushbutton','string','Solve','position',[x0 z0 w h]);

drawnow
for k = 1:10
  set(nittxt,'string',     num2str(10*k));
  set(timstptxt,'string',  num2str(5*k));
  set(restxt,'string',     num2str(1e-3*k));
  set(tperittxt,'string',  num2str(0.023*k));
  set(elaptimtxt,'string', num2str(k));
  pause(2)
end


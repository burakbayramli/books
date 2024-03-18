gui.explparams = explparams;
etimestepedit =...
 [editbleft      328
 editbleft+editw 328
 editbleft+editw 328-edith
 editbleft       328-edith 
 gui.etimestepedit = etimestepedit ;
evis2edit=...
 [editbleft       364
  editbleft+editw 364
  editbleft+editw 364-edith
  editbleft       364-edith];
  gui.evis2edit = evis2edit;
evis4edit=...
 [editbleft       400
  editbleft+editw 400
  editbleft+editw 400-edith
  editbleft       400-edith];
  gui.evis4edit=evis4edit;
ecfledit=...
 [editbleft      436
 editbleft+editw 436
 editbleft+editw 436-edith
 editbleft       436-edith];
 gui.ecfledit=ecfledit;
eplotintedit=...
 [editbleft 472
 editbleft+editw 472
 editbleft+editw 472-edith
 editbleft       472-edith];
gui.eplotintedit=eplotintedit;
tmp = {explparams eplotintedit ecfledit evis4edit evis2edit etimestepedit};
plquad(tmp)

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
gui.shkposedit =   shkposedit;
  p01edit = ...
  [editbleft      74
  editbleft+editw 74
  editbleft+editw 74-edith
  editbleft       74-edith];
gui.p01edit =  p01edit;
  t01edit = ...
  [editbleft      110
  editbleft+editw 110
  editbleft+editw 110-edith
  editbleft       110-edith];
  gui.t01edit = t01edit ;
  p2edit = ...
  [editbleft      146
  editbleft+editw 146
  editbleft+editw 146-edith
  editbleft       146-edith];
  gui.p2edit =p2edit;
shkinputradbut = [24 176];
stainputradbut = [24 201];
tmp = { shkposedit p01edit t01edit p2edit};
plquad(tmp);
tmp = {shkinputradbut stainputradbut};
plradbut(tmp)

axes2 = ...
[594 520
1240 520
1240 260
 594 260];
 gui.axes2 = axes2;

residbut = [612 561];
gui.residbut = residbut;
areabut  = [830 559];
gui.areabut = areabut;
stepsizebut = [1053 560];
gui.stepsizebut = stepsizebut;

axes1 = ...
[ 594 185
1240  185
1240  -75
 594  -75];
 gui.axes1 = axes1;
pressbut = [ 600 219];
gui.pressbut = pressbut;
velbut   = [ 820 219];
gui.velbut = velbut;
densbut  = [1040 219];
gui.densbut = densbut;
tmp = {axes1 axes2 configparams};
plquad(tmp)
tmp = {residbut areabut stepsizebut pressbut velbut densbut};
plbut(tmp)

butleft = 560; 
buth    = 40; 
butw    = 720-butleft;

solvebut = ...
[butleft     630
butleft+butw 630
butleft+butw 630-buth
butleft      630-buth];
gui.solvebut = solvebut;

resetbut = ...
[butleft     680
butleft+butw 680
butleft+butw 680-buth
butleft      680-buth];
gui.resetbut = resetbut;

exitbut = ...
[butleft     730
butleft+butw 730
butleft+butw 730-buth
butleft      730-buth];
gui.exitbut = exitbut;

tmp = {exitbut resetbut solvebut};
plquad(tmp);

infopanel = ...
[920 720
1240 720
1240 580
920  720];
gui.infopanel = infopanel;

txtl = 930;
txtr = 1230;
txth = 20;
dztxt = 25;
z0    = 627;

nittxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.nittxt = nittxt;

elaptimtxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.elaptimtxt = elaptimtxt;

tperittxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.tperittxt = tperittxt;

restxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.restxt = restxt;

timstptxt = ...
[txtl  z0
txtr  z0
txtr  z0-txth
txtl z0-txth];
z0 = z0+dztxt;
gui.timstptxt = timstptxt;
tmp = {nittxt timstptxt restxt tperittxt elaptimtxt};
plquad(tmp)

uipan = ...
[ -5 740
1270 740
1270 -85
  -5 -85];
gui.uipan = uipan;
tmp = {uipan};
plquad(tmp)


f = figure;
edithght = 20
ax = axes('position',[0.3 0.3 0.5 0.5]);
p = uipanel("title","Configuration", "position", [0.1 0.2 0.7 0.7]);
editz0 = 10
editx0 = 10
editlen = 120
editdz = 5
rektt = [editx0 editz0 editlen edithght ];
byshk  = uicontrol(p,'style','radiobutton','string','By shock pos','position',rektt)
editz0 = editz0 + edithght + editdz;
byBC   = uicontrol(p,'style','radiobutton','string','By BC.','position',rektt)
editz0 = editz0 + edithght + editdz;
editt01 = uicontrol(p, "style", "edit", "string", "T01", "position", rektt);
editz0  = editz0 + edithght + editdz;
editp01 = uicontrol(p, "style", "edit", "string", "P01", "position", rektt);
editz0  = editz0 + edithght + editdz;
editp2  = uicontrol(p, "style", "edit", "string", "P2", "position", rektt);
editz0  = editz0 + edithght + editdz;
editShockPos = uicontrol(p, "style", "edit", "string", "Shock pos", "position", rektt);
editz0  = editz0 + edithght + editdz;
fedbck  = uicontrol(p,'style','text', 'string', num2str(15),'position', rektt );
axes(ax)
plot(1:10,1:10,'k')

%while 1
  tmp = get(editp01,'string');
  tt  = str2num(tmp);
  pause(2)
  if tt == 0
    set(fedbck,'string','zero')
    break
  else
    set(fedbck,'string',num2str(tt));
  end
%end

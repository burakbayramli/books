function[]=definitions();
h=figure(101);
text(0,1.00,'CL_\alpha = \partial CL / \partial \alpha');
text(0,0.95,'CL_\beta = \partial CL / \partial \beta');
text(0,0.90,'CL_\delta = \partial CL / \partial \delta');
text(0,0.85,'CL_P = \partial CL 2V/ (\partial Pb)');
text(0,0.80,'CL_Q = \partial CL 2V/ (\partial Q C_{mac})');
text(0,0.75,'CL_R = \partial CL 2V/ (\partial R b)');

text(0,0.65,'CD_\alpha = \partial CD / \partial \alpha');
text(0,0.60,'CD_\beta = \partial CD / \partial \beta');
text(0,0.55,'CD_\delta = \partial CD / \partial \delta');
text(0,0.5,'CD_P = \partial CD 2V/ (\partial P b)');
text(0,0.45,'CD_Q = \partial CD 2V/ (\partial Q C_{mac})');
text(0,0.4,'CD_R = \partial CD 2V/ (\partial R b)');

text(0,0.3,'CY_\alpha = \partial CY / \partial \alpha');
text(0,0.25,'CY_\beta = \partial CY / \partial \beta');
text(0,0.2,'CY_\delta = \partial CY / \partial \delta');
text(0,0.15,'CY_P = \partial CY 2V/ (\partial P b)');
text(0,0.10,'CY_Q = \partial CY 2V/ (\partial Q C_{mac})');
text(0,0.05,'CY_R = \partial CY 2V/ (\partial R b)');

text(0.5,1.00,'Cl_\alpha = \partial Cl / \partial \alpha');
text(0.5,0.95,'Cl_\beta = \partial Cl / \partial \beta');
text(0.5,0.90,'Cl_\delta = \partial Cl / \partial \delta');
text(0.5,0.85,'Cl_P = \partial Cl 2V/ (\partial Pb)');
text(0.5,0.80,'Cl_Q = \partial Cl 2V/ (\partial QC_{mac})');
text(0.5,0.75,'Cl_R = \partial Cl 2V/ (\partial Rb)');

text(0.5,0.65,'Cm_\alpha = \partial Cm / \partial \alpha');
text(0.5,0.60,'Cm_\beta = \partial Cm / \partial \beta');
text(0.5,0.55,'Cm_\delta = \partial Cm / \partial \delta');
text(0.5,0.5,'Cm_P = \partial Cm 2V/ (\partial P b)');
text(0.5,0.45,'Cm_Q = \partial Cm 2V/ (\partial Q C_{mac})');
text(0.5,0.4,'Cm_R = \partial Cm 2V/ (\partial Rb)');

text(0.5,0.3,'Cn_\alpha = \partial Cn / \partial \alpha');
text(0.5,0.25,'Cn_\beta = \partial Cn / \partial \beta');
text(0.5,0.2,'Cn_\delta = \partial Cn / \partial \delta');
text(0.5,0.15,'Cn_P = \partial Cn 2V/ (\partial Pb)');
text(0.5,0.10,'Cn_Q = \partial Cn 2V/ (\partial Q C_{mac})');
text(0.5,0.05,'Cn_R = \partial Cn 2V/ (\partial R b)');
set(h,'ToolBar','none');
set(h,'MenuBar','none');
set(h,'Color','White');
set(h,'Name','Tornado derivative definitions.');


axis off


A=imread('definitions.jpg');
h=figure(102);
set(gca,'Position',[0 0 1 1])
set(h,'ToolBar','none');
set(h,'MenuBar','none');
set(h,'Color','White');
set(h,'Name','Tornado axis definitions.');
axis off
h2=image(A);
axis off
function test_layout_dbn()
% TEST_LAYOUT_DBN		Script to test some DBN layouts
%

% Change History :
% Date		Time		Prog	Note
% 17-Apr-2000	 2:40 PM	ATC	Created under MATLAB 5.3.1.29215a (R11.1)

% ATC = Ali Taylan Cemgil,
% SNN - University of Nijmegen, Department of Medical Physics and Biophysics
% e-mail : cemgil@mbfys.kun.nl 

disp('draw mhmm1')
clf
set(gcf, 'pos', [0   0   1024   600]);

intra = zeros(3);
intra(1,[2 3]) = 1;
intra(2,3) = 1;
inter = zeros(3);
inter(1,1) = 1;
n = 3;
dnodes = [1 2];
isbox = zeros(n,1); isbox(dnodes) = 1;
unfold = 4;
draw_layout_dbn(intra, inter, 0, unfold, {'Q', 'M', 'Y'}, isbox);

pause

disp('draw water1')
clf
ss = 12;
intra = zeros(ss);
intra(1,9) = 1;
intra(3,10) = 1;
intra(4,11) = 1;
intra(8,12) = 1;

inter = zeros(ss);
inter(1, [1 3]) = 1;
inter(2, [2 3 7]) = 1;
inter(3, [3 4 5]) = 1;
inter(4, [3 4 6]) = 1;
inter(5, [3 5 6]) = 1;
inter(6, [4 5 6]) = 1;
inter(7, [7 8]) = 1;
inter(8, [6 7 8]) = 1;

unfold = 3;

[dummyx, dummyy, h] = draw_layout_dbn(intra, inter,1, unfold);

col = rand(size(h,1),3);
for i=1:length(h),
  col = rand(1,3);
  % patches
  set(h(i,2),'facecolor', col); drawnow;
  % text
  set(h(i,1),'color', 1-col); drawnow;
end;
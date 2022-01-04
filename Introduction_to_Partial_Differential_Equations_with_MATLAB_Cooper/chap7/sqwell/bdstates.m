
%                      Program bdstates (bound states)
%   This program computes the boundstates for the square well potential.
%   It is used to make the graphs in figures 6.4, 6.5, and 6.6.
%   Input is the value Q ( the height of the potential walls) 
%   Requires the function mfiles pp.m  and qq.m to find the eigen
%   values. Program then plots the eigenfunction.

xleft = -2: .01 : -1;
xcenter = -1: .01: 1;
xright = 1: .01 : 2;

Q = input(' Enter the value of Q  ')
global Q

disp(' Enter 1 for a root on the upper branch p(s)  ')
m = input(' Enter 2 for a root on the lower branch q(s)    ')

sguess = input(' Enter your first guess for the root you seek ')

if m == 1

    ss = fzero('pp', sguess)

    fprintf('The value of the root is %g. \n',ss)
    fprintf('The corresponding eigenvalue lambda is %g.\n' , ss^2)

    tau =sqrt(Q-ss^2);
    yright = cos(ss)*exp(-(xright -1)*tau );
    ycenter = cos(ss*xcenter);
    yleft = fliplr(yright);


else

   ss = fzero('qq', sguess)

   fprintf(' The value of the root is %g.\n' , ss)
   fprintf(' The corresponding eigenvalue lambda is %g.\n', ss^2)

   tau = sqrt(Q-ss^2);
   yright = sin(ss)*exp(-(xright -1)*tau);
   ycenter = sin(ss*xcenter);
   yleft = -fliplr(yright);

end
plot(xright, yright)
hold on
plot(xcenter, ycenter)
plot(xleft, yleft)


plot([-2 2], [0,0], '--')
plot([0,0], [-2 2], '--')




axis([ -2 2 -2 2])



hold off





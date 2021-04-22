function y = frayritz10_8stiff3(x)
global xi xip;
y=(xip-x).*(x-xi)*6;
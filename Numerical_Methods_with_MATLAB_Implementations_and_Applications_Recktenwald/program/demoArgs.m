function [out1,out2] = demoArgs(in1,in2,in3)
% demoArgs  Variable numbers of input and output parameters
%
% Synopsis:  demoArgs
%            demoArgs(in1)
%            demoArgs(in1,in2)
%            demoArgs(in1,in2,in3)
%            out1 = demoArgs(in1,in2,in3)
%            [out1,out2] = demoArgs(in1,in2,in3)
%
% Input:   in1,in2,in3 = optional dummy input arguments
%
% Output:  out1, out2 = optional dummy input arguments
%          If input arguments are provided, out1 is the sum
%          of the inputs, and out2 is the product of the inputs

if nargin == 0                          % process optional inputs
  disp('no input arguments');  return;
elseif nargin == 1
  disp('one input argument');
  sumin = in1;  prodin = in1;
elseif nargin == 2
  disp('two input arguments');
  sumin = in1+in2;  prodin = in1*in2;
elseif nargin == 3
  disp('three input arguments');
  sumin = in1+in2+in3;  prodin = in1*in2*in3;
else
  error('Too many inputs to demoArgs');
end

if nargout==0                           % process optional outputs
  return;
elseif nargout==1
  out1 = sumin;
else
  out1 = sumin;  out2 = prodin;
end

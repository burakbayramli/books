function a = yesNoAnswer(msg)
% yesNoAnswer  Prompt user with a question, and return 1 for 'yes' and 0 for 'no'
%
% Synopsis:  a = yesNoAnswer
%            a = yesNoAnswer(msg)
%
% Input:  msg = (string) prompt message printed to command window
%               Default:  msg = 'Continue?'
%
% Output: a = 1 if user enters 'Y', 'y', or any word beginning with 'Y' or 'y',
%             otherwise, a = 0
if nargin<1, msg = 'Continue?';  end
r = input(sprintf('\n\t%s\n\t(''y'' for yes, ''n'' for no)\n',msg),'s');
if isempty(r) | lower(r(1)) == 'n'
  a = 0;
else
  a = 1;
end

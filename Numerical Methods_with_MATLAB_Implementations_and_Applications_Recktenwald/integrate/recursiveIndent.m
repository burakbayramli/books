function recursiveIndent(maxLevel,level)
% recursiveIndent  Demonstration of a recursive function
%
% Synopsis:  recursiveIndent
%            recursiveIndent(maxLevel)
%
% Input:     maxLevel = (optional) number of recursive levels
%                       Default:  maxLevel=5
%
% Output:   Print out of indented plus signs and minus signs
%           corresponding to indentation level.

if nargin<1,  maxLevel = 5;  end   %  supply default
if nargin<2,  level = 0;     end   %  initialize

if level<maxLevel
  % --- indent two spaces per level, and print plus sign
  for k=1:level,  fprintf('  ');  end
  fprintf('+%d\n',level);

  level = level + 1;
  recursiveIndent(maxLevel,level);   %  recursive call to self

  % --- indent two spaces per level, and print minus sign
  for k=1:level,  fprintf('  ');   end
  fprintf('-%d\n',level);
end

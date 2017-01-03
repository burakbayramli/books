function K = labreadkey(F)
% K = labreadkey(F)  Read a phone key file
%      F is the filename of a 'phone key file', which defines a set of 
%      phone symbols in a particular order.  K is returned as a char 
%      matrix holding the names of each phone, with the row order 
%      defining the numerical code that will be used for that phone.
% 2001-03-27 dpwe@ee.columbia.edu

fid = fopen(F,'rt');

eof = 0;

nsym = 0;
cursym = 0;
maxlen = 0;

while eof == 0

  line = fgetl(fid);
  if ~isstr(line)
    eof = 1;
  else
    spcpos = find(line == ' ');
    if length(spcpos) == 0
      % No space in this line (including empty line)
      nn = str2num(line);
      if length(nn) > 0 & nsym == 0
        % This was the header symbol count
        nsym = nn;
        K = ' '*ones(nsym, 1);
        maxlen = 1;
        sym = '';
      else
        sym = line;
      end
    else
      % Found a space - must be 'sym indx' pairs
      sym = line([1:(spcpos(1)-1)]);
      idx = str2num(line([(spcpos(1)+1):length(line)]));
      if idx ~= cursym
        disp(['Symbol ',sym,' has index ',num2str(idx),' but expecting ',num2str(cursym)]);
      end
    end
    ls = length(sym);
    if ls > 0
      % Maybe add this symbol to the array
      if ls > maxlen
        % Pad out existing symbols
        K = [K,' '*ones(max(cursym, nsym),ls - maxlen)];
        maxlen = ls;
      end
      if ls < maxlen
        sym = [sym, ' '*ones(1,maxlen - ls)];
      end
      cursym = cursym+1;
      K(cursym,:) = sym;
    end
  end

end

fclose(fid);

% Make sure it's typed as chars
K = char(K);


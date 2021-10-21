

% plot finer grid solution from subdirectory qref

   dir = './qref/';
   dim = 1;
   [qrefdata,t1] = readamrdata(dim,Frame,dir);
   if isempty(t1)
      disp('Run xclaw in qref to generate reference solution')
    else
      hold on;
      if UserVariable
         [qref,xref] = plotframe1ez(qrefdata,mq,'b-',UserVariableFile);
       else
         [qref,xref] = plotframe1ez(qrefdata,mq,'b-');
       end
      hold off;

      % compute and print 1-norm of error: compare to coarsened qref
      mxref = length(qref);
      ratio = mxref/mx;
      qref1 = coarsen(qref,ratio);
      err1 = sum(abs(q-qref1)) * dx
    end


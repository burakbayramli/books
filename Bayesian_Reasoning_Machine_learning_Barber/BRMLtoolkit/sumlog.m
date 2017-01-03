function lx=sumlog(x)
%SUMLOG sum(log(x)) with a cutoff at 10e-200
cutoff=10e-200;x(find(x<cutoff))=cutoff; lx=sum(log(x));
from gPy.Parameters import Factor, CPT
from gPy.Models import BNM

def _main():
    # values for all variables
    vals = 0,1
    cpts = []
    for i in range(1,9):
        # construct variable names
        vu = 'U'+str(i)
        vr = 'R'+str(i)
        vx = 'X'+str(2*i-1),'X'+str(2*i)
        vy = 'Y'+str(2*i-1),'Y'+str(2*i)
        
        # create input Ui CPT
        cpts.append(CPT(
            Factor([vu],[0.5,0.5],new_domain_variables={vu:vals})
            ,vu))

        # Create CPTs for R and two X variables with dummy data
        if i == 1:
            r_parents = [vr,vu]
        else:
            r_parents = [vr,vu,'R'+str(i-1)]
        tmp_cpts = []
        tmp_cpts.append(CPT(Factor(r_parents,new_domain_variables={vr:vals}),vr))
        tmp_cpts.append(CPT(Factor([vu,vx[0]],new_domain_variables={vx[0]:vals}),vx[0]))
        tmp_cpts.append(CPT(Factor([vu,vr,vx[1]],new_domain_variables={vx[1]:vals}),vx[1]))

        # put in correct data for R and two X variables
        for cpt in tmp_cpts:
            data_it = cpt.parent_insts_indices()
            for pi in cpt.parent_insts():
                out = sum(pi)%2
                data_indices = data_it.next()
                if out == 0:
                    cpt._data[data_indices[0]] = 1.0
                    cpt._data[data_indices[1]] = 0.0
                else:
                    cpt._data[data_indices[0]] = 0.0
                    cpt._data[data_indices[1]] = 1.0
        cpts.extend(tmp_cpts)
        for j, y in enumerate(vy):
            cpts.append(CPT(Factor([vx[j],y],[0.6,0.4,0.4,0.6],new_domain_variables={y:vals}),y))
    return BNM(cpts)

codecbn = _main()

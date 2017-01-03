
def gibbs_sample(self,iterations,burnin):

    from gPy.Parameters import CPT
    from gPy.Samplers import MultinomialSampler
    
    bns = []
    inst = []
    parent_pos = []
    vs = sorted(self.variables())
    for variable in vs:
        factor = 1
        for f in self.factors_containing_variable(variable):
            factor *= f
        cpt = CPT(factor,variable,cpt_force=True,allow_dummies=True)
        parent_insts_data = cpt.parent_insts_data()
        vals = sorted(cpt.values(variable))
        dkt = {}
        for parent_inst in cpt.parent_insts():
            dkt[parent_inst] = MultinomialSampler(
                dict(zip(vals,parent_insts_data.next())))
        bns.append(dkt)
        inst.append(sorted(self.values(variable))[0])
        this_parent_pos = []
        for i, v in enumerate(vs):
            if v in cpt.parents():
                this_parent_pos.append(i)
        parent_pos.append(this_parent_pos)

    vis = range(len(vs))
    for i in range(burnin):
        for j in vis:
            parent_inst = [inst[pos] for pos in parent_pos[j]]
            inst[j] = bns[j][tuple(parent_inst)].sample()
    insts = []
    for i in range(iterations):
        for j in vis:
            parent_inst = [inst[pos] for pos in parent_pos[j]]
            inst[j] = bns[j][tuple(parent_inst)].sample()
        insts.append(tuple(inst))
    return tuple(insts)

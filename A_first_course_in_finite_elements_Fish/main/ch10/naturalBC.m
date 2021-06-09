% compute and assemble nodal boundary force vector
function f = naturalBC(f);
include_flags;
for i = 1:neq
    if flags(i) == 1
        dof = ID(i);
        f(dof) = f(dof) + n_bc(dof);
    end
end
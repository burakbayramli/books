"""
Change q_l to q_\ell and all other _l to _\ell in markdown cells.
Skips over code cells since q_l and q_left are also used in code and should
not be changed.

Not perfect: there is a `q_left` in a markdown cell that gets changed
and has to be manually changed back.
"""

part0 = ['Preface']

part1 = ['Introduction',
            'Advection',
            'Acoustics',
            'Burgers',
            'Traffic_flow',
            'Nonconvex_scalar',
            'Shallow_water',
            'Shallow_tracer',
            'Euler']

part2 = ['Approximate_solvers',
         'Burgers_approximate',
         'Shallow_water_approximate',
         'Euler_approximate',
         'FV_compare']

chapters = part0 + part1 + part2
files = [f+'.ipynb' for f in chapters]

#files = ['Shallow_water.ipynb']

for f in files:
    print('FILE: ',f)
    lines = open(f).readlines()
    markdown_line = False
    with open(f, 'w') as file:
        for line in lines:
            if 'cell_type' in line:
                markdown_line = ('markdown' in line)
            if markdown_line:
                if ('_l' in line) and ('_lex' not in line):
                    line = line.replace('_l',r'_\\ell')
                #line = line.replace(r'_\\ell',r'_l')  # to change back
            file.write(line)




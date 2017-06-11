pac_types_str="""
C = Communication Cost
D = Delegate
E = Electioneering Communication
H = House
I = Independent Expenditor (Person or Group)
N = PAC - Nonqualified
O = Independent Expenditure-Only (Super PACs)
P = Presidential
Q = PAC - Qualified
S = Senate
U = Single Candidate Independent Expenditure
X = Party Nonqualified
Y = Party Qualified
Z = National Party Nonfederal Account 
""".replace(' = ','=').strip().split('\n')

pac_types=dict([tuple(row.split('=')) for row in pac_types_str])
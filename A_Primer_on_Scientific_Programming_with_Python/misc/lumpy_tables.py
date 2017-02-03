from scitools.Lumpy import Lumpy

# Table of columns
lumpy = Lumpy() 
lumpy.make_reference()
Cdegrees = [20, 25, 30, 35, 40]
Fdegrees = [(9.0/5)*C + 32 for C in Cdegrees]
table1 = [Cdegrees, Fdegrees]
del Cdegrees, Fdegrees, C
lumpy.object_diagram()

# Table of rows
lumpy = Lumpy() 
Cdegrees = [20, 25, 30, 35, 40]
Fdegrees = [(9.0/5)*C + 32 for C in Cdegrees]
table2 = [[C, F] for C, F in zip(Cdegrees, Fdegrees)]
del Cdegrees, Fdegrees, C, F
lumpy.object_diagram()


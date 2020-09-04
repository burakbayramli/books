# name_collision.py
# -------------------------------------------------------------------------
# Demonstrate how Python's rules of scope prevent name collisions.
# ------------------------------------------------------------------------- 

def name_collisions():
	x, y = 'E', 'E'
	def display():
		x = 'L'
		print( "Inside display() ..." )
		print( "x= {}\ny= {}\nz= {}".format(x,y,z) )
	display()
	print( "Inside name_collision() ..." )
	print( "x= {}\ny= {}\nz= {}".format(x,y,z) )

x, y, z = 'G', 'G', 'G'
name_collisions()
print( "Outside function ..." )
print( "x= {}\ny= {}\nz= {}".format(x,y,z) )

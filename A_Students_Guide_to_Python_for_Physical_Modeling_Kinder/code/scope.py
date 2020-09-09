# scope.py 
# -------------------------------------------------------------------------
# Demonstrate Python's rules of scope.
# ------------------------------------------------------------------------- 

def illustrate_scope():
	s_enclosing = 'E'
	def display():
		s_local = 'L'
		print( "Local --> {}".format(s_local) )
		print( "Enclosing --> {}".format(s_enclosing) )
		print( "Global --> {}".format(s_global) )
		print( "Built-in --> {}".format(abs) )
	display()

s_global = 'G'

illustrate_scope()

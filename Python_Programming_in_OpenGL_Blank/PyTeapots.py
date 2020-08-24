# teapots.py
# by Allen Downey

# The following program demonstrates features from the
# first few chapters of the Red Book.  I have lifted
# lines from lots of different examples, with the intention
# of getting it all in one place.

# The documentation for PyOpenGL, GLU and GLUT is at:

#http://pyopengl.sourceforge.net/documentation/manual/reference-GL.xml
#http://pyopengl.sourceforge.net/documentation/manual/reference-GLU.xml
#http://pyopengl.sourceforge.net/documentation/manual/reference-GLUT.xml

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy import *
import sys
from time import sleep

class Object:
	def step(self): pass

class Sphere(Object):
	def __init__(self, world, size=1, rows=32, cols=32):
		self.size = size
		self.rows = rows
		self.cols = cols
		world.add_opaque(self)
		
	def display(self):
		glutSolidSphere(self.size, self.rows, self.cols)


class Teapot(Object):
	def __init__(self, world, size=1, orbit=5, orbit_angle=0, rot_angle=0,
				rev_speed=1, rot_speed=2):
		self.size = size
		self.orbit = orbit
		self.orbit_angle = orbit_angle
		self.rot_angle = rot_angle
		self.rev_speed = rev_speed
		self.rot_speed = rot_speed
		self.rotation = 0
		self.revolution = 0
		world.add_opaque(self)

	def step(self):
		# update the state of the object
		self.rotation = (self.rotation + self.rot_speed) % 360
		self.revolution = (self.revolution + self.rev_speed) % 360

	def display(self):
		# precondition: matrix mode is modelview
		# invariant: restores the current matrix
		glPushMatrix()

		axis = (0, 0, 1)
		rev_axis=(0, 1, 0)
		rot_axis=(0, 1, 0)
		
		glRotate(self.orbit_angle, *axis)
		glRotate(self.revolution, *rev_axis)
		glTranslate(self.orbit, 0, 0)
		glRotate(self.rot_angle, *axis)
		glRotate(self.rotation, *rot_axis)

		glutSolidTeapot(self.size)

		self.frame = get_frame()
		glPopMatrix()


class Triangle(Object):
	def __init__(self, world):
		world.add_transparent(self)

	def display(self):
		# most basic use of Begin, End and Vertex to
		# draw a polygon
		glBegin(GL_TRIANGLES)
		glColor(1.0, 1.0, 1.0)
		glVertex(0, 0, 0)
		glVertex(5, 5, 0)
		glVertex(-5, 5, 0)
		glEnd()
		

class Orbit(Object):
	def __init__(self, world, orbit=5, orbit_angle=0, n=40):
		self.orbit = orbit
		self.orbit_angle = orbit_angle
		self.n = n
		world.add_transparent(self)

	def display(self):
		# precondition: matrix mode is modelview
		# invariant: restores the current matrix

		glPushMatrix()
		axis = (0, 0, 1)
		glRotate(self.orbit_angle, *axis)

		vertices = self.compute_vertices()
		glBegin(GL_TRIANGLE_FAN)
		for vertex in vertices:
			glColor(0, 0, 1, 0.1)
			glVertex(vertex)
		glEnd()
		
		glPopMatrix()

	def compute_vertices(self):
		glPushMatrix()
		
		axis = (0, 1, 0)
		vertices = [(0, 0, 0)]
		for i in range(self.n+1):
			glLoadIdentity()
			angle = i * 360.0/self.n
			glRotate(angle, *axis)
			glTranslate(0, 0, self.orbit)
			vertices.append(whereami())
		glPopMatrix()
		return vertices

def whereami():
	# get the translation part of the model-view matrix
	frame = glGetFloat(GL_MODELVIEW_MATRIX)
	return frame[3]    

def get_frame():
	# get the model-view matrix
	return glGetFloat(GL_MODELVIEW_MATRIX)

def print_frame(frame):
	print transpose(array(frame))

def print_current_frame():
	print_frame(get_frame())

class Vlist(list):
	def __init__(self, type=GL_TRIANGLE_FAN):
		self.type = type
		world.add_opaque(self)

	def display(self):
		glBegin(GL_TRIANGLE_FAN)
		for vertex in self:
			glVertex(vertex)
		glEnd()


class World:
	def __init__(self, args, size=(500, 500), pos=(100, 100), bg=(1, 1, 1, 0)):
		self.args = args
		self.size = size
		self.pos = pos
		self.bg = bg
		self.opaques = []
		self.transparents = []
		self.fog = Fog()

		self.set_up_gl()
		self.camera = Camera(position=(0, 3, 10))
		self.lights = []
		self.add_lights()

	def add_opaque(self, object):
		self.opaques.append(object)

	def add_transparent(self, object):
		self.transparents.append(object)

	def set_up_gl(self):
		# process command-line arguments
		glutInit(self.args)

		# turn on double-buffering and rgb color
		glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA)

		# create the window
		glutInitWindowSize(*self.size)
		glutInitWindowPosition(*self.pos)
		glutCreateWindow(self.args[0])

		# clear the background
		glClearColor(*self.bg)

		glShadeModel(GL_SMOOTH)
		glEnable(GL_DEPTH_TEST)
		glEnable(GL_LIGHTING)
#        glLightModel(GL_LIGHT_MODEL_LOCAL_VIEWER, 1)
#        glLightModel(GL_LIGHT_MODEL_TWO_SIDE, 1)

		glEnable(GL_BLEND)
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

		# set up the callbacks
		glutDisplayFunc(self.display)
		glutReshapeFunc(self.reshape)
		glutKeyboardFunc(self.keyboard)
		glutMouseFunc(self.mouse)
		glutIdleFunc(self.idle)


	def add_lights(self):
		# put a red light on the camera
		position = self.camera.position + (0,)
		color = (1, 0, 0, 0)
		light = Light(GL_LIGHT0, position, color)
		self.lights.append(light)

		# put a blue light on the right
		position = (10, 0, 0, 0)
		color = (0, 0, 1, 0)
		light = Light(GL_LIGHT1, position, color)
		self.lights.append(light)

		# put a green light at high noon
		position = (0, 10, 0, 0)
		color = (0, 1, 0, 0)
		light = Light(GL_LIGHT2, position, color)
		self.lights.append(light)


	def display(self):
		# precondition: matrix mode is modelview
		# invariant: restores the current matrix

		glMaterial(GL_FRONT, GL_SPECULAR, (1.0, 1.0, 1.0, 0.15))
		glMaterial(GL_FRONT, GL_SHININESS, (100.0, ))
		
		# clear out the colors and the buffers
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

		# you have to draw the fog every time
		if hasattr(self, 'fog'):
			self.fog.setup()

		# display all the objects
		glMaterial(GL_FRONT, GL_EMISSION, ( 0.0, 0.0, 0.0, 1.0))
		glMaterial(GL_FRONT, GL_DIFFUSE, (0.75, 0.75, 0.0, 1.0))

		for object in self.opaques:
			try:
				object.display()
			except:
				(type, value, traceback) = sys.exc_info()
				print type, value
				sys.exit()

		glMaterial(GL_FRONT, GL_EMISSION, ( 0.0, 0.3, 0.3, 0.6))
		glMaterial(GL_FRONT, GL_DIFFUSE, ( 0.0, 0.8, 0.8, 0.8))
		glDepthMask(GL_FALSE)

		for object in self.transparents:
			object.display()
		glDepthMask(GL_TRUE)

		# reveal the finished picture
		glutSwapBuffers()


	def reshape (self, w, h):
		glViewport (0, 0, w, h)
		self.camera.point()


	def keyboard(self, key, x, y):
		# quit if the user presses Control-C
		if key == chr(3):
			sys.exit(0)


	def mouse(self, button, state, x, y):
		if button == GLUT_LEFT_BUTTON:
			if state == GLUT_DOWN:
				glutIdleFunc(None)
			elif state == GLUT_UP:
				glutIdleFunc(self.idle)
	
	def idle(self):
		for object in self.opaques:
			object.step()

		for object in self.transparents:
			object.step()

		# mark the scene for redisplay during the next iteration
		# of mainLoop
		glutPostRedisplay()
		sleep(0.005)

	def mainloop(self):
		glutMainLoop()


class Camera:
	def __init__(self, position=(0, 0, 10), target=(0, 0, 0), up=(0, 1, 0),
				fovy=60, aspect=1, near=2, far=20):
		self.position = position
		self.target = target
		self.up = up
		self.fovy = fovy
		self.aspect = aspect
		self.near = near
		self.far = far
		self.point()
		
	def point(self):
		# postcondition: matrix mode is modelview
		
		# set up the projection matrix
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity()
		gluPerspective(self.fovy, self.aspect, self.near, self.far)

		# set up the modelview matrix
		glMatrixMode(GL_MODELVIEW)
		glLoadIdentity()
		gluLookAt(*self.position + self.target + self.up)

class Fog:
	def __init__(self, color=(1, 1, 1, 1), mode=GL_LINEAR,
				density=0.1, hint=GL_DONT_CARE, start=2, end=20):
		self.color = color
		self.mode = mode
		self.density = density
		self.hint = hint
		self.start = start
		self.end = end
		self.setup()

	def setup(self):
		glEnable(GL_FOG)
		glFog(GL_FOG_MODE, self.mode)
		glFog(GL_FOG_COLOR, self.color)
		glFog(GL_FOG_DENSITY, self.density)
		glFog(GL_FOG_START, self.start)
		glFog(GL_FOG_END, self.end)       
		glHint(GL_FOG_HINT, self.hint)


class Light:
	def __init__(self, name, position=(0, 0, 1, 0), color=(1, 1, 1, 1)):
		self.name = name
		self.position = position
		self.acolor = color
		self.dcolor = color
		self.scolor = color
		self.setup()

	def setup(self):
		glLightfv(self.name, GL_AMBIENT, self.acolor)
		glLightfv(self.name, GL_DIFFUSE, self.dcolor)
		glLightfv(self.name, GL_SPECULAR, self.scolor)
		glLightfv(self.name, GL_POSITION, self.position)
		glEnable(self.name)


def main(*args):
	world = World(args)

	sphere = Sphere(world, 2)

	for i in range(4):
		teapot = Teapot(world, orbit = 7.2, orbit_angle = i*90)
		teapot.revolution = i*90
	
	plane = Orbit(world, orbit = 7.2)
	tri = Triangle(world)

	world.mainloop()

if __name__ == "__main__":
	main(*sys.argv)
	


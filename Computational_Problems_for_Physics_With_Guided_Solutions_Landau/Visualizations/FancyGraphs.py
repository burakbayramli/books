""" From "COMPUTATIONAL PHYSICS" & "COMPUTER PROBLEMS in PHYSICS"
    by RH Landau, MJ Paez, and CC Bordeianu (deceased)
    Copyright R Landau, Oregon State Unv, MJ Paez, Univ Antioquia, 
    C Bordeianu, Univ Bucharest, 2017. 
    Please respect copyright & acknowledge our work."""

# FancyGraph.py:  fancy graphics written by Lensyl Urbano
 
from visual import *

class x_tick:
    "labels on x axis"
    def __init__(self, p, lab):
        self.f = frame(pos = p)
        self.tick = curve(color = (1, 0, 0), frame = self.f)
        self.tick.append(pos = (0, 0, 0) )
        self.tick.append(pos = (0, 0.03, 0) )
        self.label = label(frame=self.f, text=lab, pos =(0,-0.05,0), height=20, box=0, opacity=0)

class y_tick:
    "labels on y axis"
    def __init__(self, p, lab):
        self.f = frame(pos = p)
        self.tick = curve(color = (1, 0, 0), frame = self.f)
        self.tick.append(pos = (0, 0, 0) )
        self.tick.append(pos = (0.03, 0.0, 0) )
        self.label = label(frame = self.f, text = lab, pos = ( - 0.1, 0, 0), height = 20, box = 0, opacity = 0)

def atemp(elev, lr, stemp):
    # determine atmospheric T for  surface temp stemp and lapse rate lr
    # lapse rate in deg per 1000 m
    return (stemp - (elev*lr/1000.0) )

def ballrad(T, e):
    "determine radius of balloon"
    # at 25 C and 0.0 m the radius is rnorm and pressure is 1 bar
    # emax is half life for the pressure decrease in the atmosphere
    rnorm = 0.1
    Tnorm = 25.0 + 273.0
    Pzero = 1.0
    emax = y_max * 4.0
    P = emax**2 / (e + emax)**2
    vnorm = 4 * pi * rnorm 
    T = T + 273.0
    Vnew = T * Pzero * vnorm / (Tnorm * P)
    return Vnew/(4*pi)
    
def graphtoT(x):
    "takes the x - coord on the graph and converts it to Temperature"
    return (x * dx) + x_min

def Ttograph(T):
    "takes the Temperature and converts it to a location on the graph"
    return (T - x_min) / dx

def contcolor(T, Tmin, Tmax):
    "color ranges from blue (at minimum temperature) to red"
    Tmin = min(x_min, graphtoT(el_max_ctrl.x) )
    Tmax = max(x_max, graphtoT(y_zero_ctrl.x) )
    r = ( (T - Tmin)/(Tmax - Tmin) )
    g = 0
    b = ( (Tmax - T)/(Tmax - Tmin) )
    return vector(r, g, b)


w = 640 + 4 + 4
h = 480 + 24 + 4
scene.x = 0
scene.y = 0
scene.width = w
scene.height = h

x_max = 30.0
x_min = - 30.0
dx = x_max  -  x_min
y_max = 5000
y_axis = curve(color = (1, 0, 0) )
y_axis.append(pos = vector(0, 1, 0) )
y_axis.append(pos = vector(0, 0, 0) )
y_axis_label = label(pos = (-0.2,0.5,0), text='z(km)', height=25, border=6, box=0, opacity=0)
x_axis = curve(color = (1, 0, 0) )
x_axis.append(pos = (0, 0, 0) )
x_axis.append(pos = (1, 0, 0) )
x_axis_label = label( pos=(0.5, -0.15, 0), text='Temperature (Celcius)', height=25, border = 6, box = 0, opacity = 0)

# create x - axis labels
xlabels = []
nlabels = 4
for i in range(0, nlabels + 1):
    # print i
    xlabels.append(x_tick(vector(float(i)/float(nlabels), 0, 0), `x_min + (dx*i/float(nlabels) )`) )
# create y - axis labels
ylabels = []
nlabels = 5
for i in range(0, nlabels + 1):
    # print i
    ylabels.append(y_tick(vector(0, float(i)/float(nlabels), 0), `(y_max*i/float(nlabels)/1000.0)`) )

darate = 10.0
maxda = atemp(y_max, darate, x_max)
print `maxda`
marate = 6.0
maxma = atemp(y_max, marate, x_max)
dar = curve(color = (0, 1, 1), pos = [ (1, 0), ( (maxda - x_min)/dx, 1) ])
mar = curve(color = (0, 1, 0), pos = [ (1, 0), ( (maxma - x_min)/dx, 1) ])
dar_label = label( pos = dar.pos[1] + vector(0, 0.05, 0), text = 'DAR', height = 20, border = 6, box = 0, opacity = 0, color = dar.color[1])
mar_label = label( pos = mar.pos[1] + vector(0, 0.05, 0), text = 'MAR', height = 20, border = 6, box = 0, opacity = 0, color = mar.color[1])
erate = 12.0
maxe = atemp(y_max, erate, x_max)
elr = curve(color = (1, 1, 0), pos = [ (1, 0), ( (maxe - x_min)/dx, 1) ])
elr_label = label( pos = elr.pos[ - 1] + vector(0, 0.1, 0), text = 'ELR', height = 20, border = 6, box = 0, opacity = 0, color = elr.color[1])
r = 0.025
y_zero_ctrl = sphere(radius = r, pos = (1, 0, 0) )
el_max_ctrl = sphere(radius = r, pos = elr.pos[1])
init_elev = 1000.0
small_ball = sphere(color = (1, 1, 0), radius = r, pos = (Ttograph(atemp(init_elev, darate, graphtoT(elr.x[0]) )), init_elev/y_max, 0.001) )
els_ball = sphere(color = (1, 0, 1.0), radius = r, pos = (Ttograph(atemp(init_elev, erate, graphtoT(elr.x[0]) )), init_elev/y_max, 0) )
sb_min = Ttograph(atemp(init_elev/y_max, darate, graphtoT(elr.x[0]) ))
sb_max = Ttograph(atemp(init_elev/y_max, marate, graphtoT(elr.x[0]) ))
print init_elev
print sb_max
print sb_min
elrx = curve(color = elr.color[0], pos = [ els_ball.pos, (els_ball.pos.x, 0, els_ball.pos.z) ], visible  = 0)
elry = curve(color = elr.color[0], pos = [ els_ball.pos, (0, els_ball.pos.y, els_ball.pos.z) ], visible  = 0)
sbx = curve(color = small_ball.color, pos = [ small_ball.pos, (small_ball.pos.x, 0, small_ball.pos.z) ], visible  = 0)
sby = curve(color = small_ball.color, pos = [ small_ball.pos, (0, small_ball.pos.y, small_ball.pos.z) ], visible  = 0)
elrxy_but = sphere(radius = r*2, pos = (0.1,  - .3, 0), color = (1, 1, 1) )
elr_l = 0
sbxy_but = sphere(radius = r*2, pos = (0.2,  - .3, 0), color = (1, 1, 1) )
sb_l = 0
scene.center = vector(0.5, 0.5, 0)
scene.autoscale = 0
scene.userspin = 0
# CREATE SECOND DISPLAY
disp2 = display(x = w, y = 0, width = w, height = h, range = scene.range, autoscale = 0)
balloon = cylinder( radius = ballrad(graphtoT(small_ball.x), small_ball.y * y_max), pos = (0.5, small_ball.y, 0), axis = (0, 0, 0.01) )
balloon.color = contcolor(graphtoT(small_ball.pos.x), x_min, x_max)
# balloon.outline = ring(radius = balloon.radius, axis = (0, 0, 1), color = (0, 0, 0), pos = balloon.pos, thickness = 0.01)
it_lab = label( pos = balloon.pos, text = `round(graphtoT(small_ball.pos.x), 0)`, height = 10, box = 0, opacity = 0)
xt_lab = label( pos = (0.95, balloon.pos.y, balloon.pos.z), text = `round(graphtoT(els_ball.pos.x), 0)`, height = 10, box = 0, opacity = 0)

y_axis2 = curve(color = (1, 0, 0) )
y_axis2.append(pos = vector(0, 1, 0) )
y_axis2.append(pos = vector(0, 0, 0) )
y_axis2_label = label( pos = ( - 0.2, 0.5, 0), text = 'z(km)', height = 20, border = 6, box = 0, opacity = 0)
x_axis2 = curve(color = (1, 0, 0) )
x_axis2.append(pos = (0, 0, 0) )
x_axis2.append(pos = (1, 0, 0) )

# create y - axis labels
ylabels2 = []
for i in range(0, nlabels + 1):
    # print i
    ylabels2.append(y_tick(vector(0, float(i)/float(nlabels), 0), `(y_max*i/float(nlabels)/1000.0)`) )

# create contour map
Tmap = faces()
Tmap.append(pos = (0, 0, 0), normal = (0, 0, 1), color = contcolor(graphtoT(y_zero_ctrl.x), x_min, x_max) )
Tmap.append(pos = (1, 0, 0), normal = (0, 0, 1), color = contcolor(graphtoT(y_zero_ctrl.x), x_min, x_max) )
Tmap.append(pos = (0, 1, 0), normal = (0, 0, 1), color = contcolor(graphtoT(el_max_ctrl.x), x_min, x_max) )
Tmap.append(pos = ( 1, 1, 0), normal = (0, 0, 1), color = contcolor(graphtoT(el_max_ctrl.x), x_min, x_max) )
Tmap.append(pos = ( 0, 1, 0), normal = (0, 0, 1), color = contcolor(graphtoT(el_max_ctrl.x), x_min, x_max) )
Tmap.append(pos = ( 1, 0, 0), normal = (0, 0, 1), color = contcolor(graphtoT(y_zero_ctrl.x), x_min, x_max) )
disp2.center = vector(0.5, 0.5, 0)
sb_rate_update = 1
framerate = 100
pick = None

while 1:
    rate(framerate)
    if scene.mouse.events:
        m1 = scene.mouse.getevent() # obtain drag or drop event
#       print m1.pick
        if m1.drag and (m1.pick == y_zero_ctrl or m1.pick == el_max_ctrl or m1.pick == small_ball):
            drag_pos = m1.pickpos
            pick = m1.pick
            scene.cursor.visible = 0 # make cursor invisible
        elif m1.drop:
            pick = None # end dragging
            scene.cursor.visible = 1 # cursor visible
        elif m1.pick == sbxy_but and m1.release:
            if sb_l == 0:
                sb_l = 1
                sbxy_but.color = (1, 0, 0)
                sbx.visible = 1
                sby.visible = 1
            else:
                sbxy_but.color = (1, 1, 1)
                sb_l = 0
                sbx.visible = 0
                sby.visible = 0
        elif m1.pick == elrxy_but and m1.release:
            if elr_l == 0:
                elr_l = 1
                elrxy_but.color = (1, 0, 0)
                elrx.visible = 1
                elry.visible = 1
            else:
                elrxy_but.color = (1, 1, 1)
                elr_l = 0 
                elrx.visible = 0
                elry.visible = 0 
    if pick:
        new_pos = scene.mouse.project(normal = (0, 0, 1) )
        if new_pos != drag_pos:
            pick.pos   += new_pos  -  drag_pos
            drag_pos = new_pos
            if pick == y_zero_ctrl:
                y_zero_ctrl.y = 0
                dar.pos[0] = y_zero_ctrl.pos
                mar.pos[0] = y_zero_ctrl.pos
                elr.pos[0] = y_zero_ctrl.pos
                maxda = atemp(y_max, darate, graphtoT(dar.x[0]) )
                maxma = atemp(y_max, marate, graphtoT(mar.x[0]) )
                dar.pos[1] = vector( (maxda - x_min)/dx, 1, 0)
                mar.pos[1] = vector( (maxma - x_min)/dx, 1, 0)
                dar_label.pos = dar.pos[1] + vector(0, 0.05, 0)
                mar_label.pos = mar.pos[1] + vector(0, 0.05, 0)
                erate = (graphtoT(elr.x[0]) - graphtoT(elr.x[1]) )*1000.0/y_max
                sb_rate_update = 0
            else:
                sb_rate_update = 1
            if pick == el_max_ctrl:
                el_max_ctrl.y = 1
                elr.pos[1] = el_max_ctrl.pos
                erate = (graphtoT(elr.x[0]) - graphtoT(elr.x[1]) )*1000.0/y_max
            if pick == small_ball:
                sb_elev = small_ball.y * y_max
                sb_min = Ttograph(atemp(sb_elev, darate, graphtoT(elr.x[0]) ))
                sb_max = Ttograph(atemp(sb_elev, marate, graphtoT(elr.x[0]) ))
                if drag_pos.x < sb_min:
                    small_ball.x = sb_min
                elif drag_pos.x > sb_max:
                    small_ball.x = sb_max
                balloon.pos.y = small_ball.y
                # balloon.outline.pos = balloon.pos
                els_ball.y = small_ball.y
                els_ball.x = Ttograph(atemp(sb_elev, erate, graphtoT(elr.x[0]) ))         
    elr_label.pos = elr.pos[ - 1] + vector(0, 0.1, 0)
    elrx.pos[0] = els_ball.pos
    elrx.pos[ - 1] = vector(els_ball.pos.x, 0, els_ball.pos.z) 
    elry.pos[0] = els_ball.pos
    elry.pos[ - 1] = vector(0, els_ball.pos.y, els_ball.pos.z)
    sbx.pos[0] = small_ball.pos
    sbx.pos[ - 1] = vector(small_ball.pos.x, 0, small_ball.pos.z) 
    sby.pos[0] = small_ball.pos
    sby.pos[ - 1] = vector(0, small_ball.pos.y, small_ball.pos.z) 
    balloon.radius = ballrad(atemp(balloon.y, erate, graphtoT(elr.x[0]) ), balloon.y * y_max)
    # balloon.outline.radius = balloon.radius
    balloon.color = contcolor(graphtoT(small_ball.pos.x), x_min, x_max)
    it_lab.pos = balloon.pos
    it_lab.text = `round(graphtoT(small_ball.pos.x), 0)`
    xt_lab.pos = (0.95, balloon.pos.y, balloon.pos.z)
    xt_lab.text = `round(graphtoT(els_ball.pos.x), 0)`
    Tmap.color[0] = contcolor(graphtoT(y_zero_ctrl.x), x_min, x_max)
    Tmap.color[1] = contcolor(graphtoT(y_zero_ctrl.x), x_min, x_max)
    Tmap.color[2] = contcolor(graphtoT(el_max_ctrl.x), x_min, x_max)
    Tmap.color[3] = contcolor(graphtoT(el_max_ctrl.x), x_min, x_max)
    Tmap.color[4] = contcolor(graphtoT(el_max_ctrl.x), x_min, x_max)
    Tmap.color[5] = contcolor(graphtoT(y_zero_ctrl.x), x_min, x_max)
    if sb_rate_update == 1:
        sb_elev = small_ball.y * y_max
        sb_rate = (graphtoT(elr.x[0])  -  graphtoT(small_ball.x) ) / (sb_elev/1000.0)                
    small_ball.pos = (Ttograph(atemp(sb_elev, sb_rate, graphtoT(elr.x[0]) )), small_ball.y, 0.01)    
    els_ball.pos = (Ttograph(atemp(els_ball.y * y_max, erate, graphtoT(elr.x[0]) )), small_ball.y, 0)

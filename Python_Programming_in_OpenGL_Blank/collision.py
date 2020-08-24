#########################################
# Import the library(s)
#########################################
from visual import *
from random import uniform


##########################################
# Create Wall(s)
##########################################
thk = 0.3
side = 4.0
s2 = 2*side - thk
s3 = 2*side + thk

wallR = box (pos=vector( side, 0, 0), length=thk, height=s2, width=s3, color = color.red)
wallL = box (pos=vector(-side, 0, 0), length=thk, height=s2, width=s3, color = color.red)
wallB = box (pos=vector(0, -side, 0), length=s3, height=thk, width=s3, color = color.blue)
wallT = box (pos=vector(0, side, 0), length=s3, height=thk, width=s3, color = color.blue)
wallBK = box(pos=vector(0, 0, -side), length=s2, height=s2, width=thk, color = (0.7,0.7,0.7))


##########################################
# Create Ball(s)
##########################################
no_particles=30
ball_radius=0.250
maxpos=side-.5*thk-ball_radius
maxv=1.0

ball_list=[]
for i in arange(no_particles):
    ball=sphere(color=color.green,radius=ball_radius)
    ball.pos=maxpos*vector(uniform(-1,1),uniform(-1,1),uniform(-1,1))
    ball.velocity=maxv*vector(uniform(-1,1),uniform(-1,1),uniform(-1,1))
    ball_list.append(ball)


##########################################
# Time loop for moving Ball(s)
###########################################
timestep = 0.05


while (1==1):
    rate(100)

    ######################################
    # Loop over list of particles
    # Move and check wall collisions
    ######################################
    for ball in ball_list:

        #move ball
        ball.pos = ball.pos + ball.velocity*timestep

        #check right wall collisions
        if ball.x > maxpos:
            #reflect velocity
            ball.velocity.x = -ball.velocity.x
            #reflect position
            ball.x=2*maxpos-ball.x
        #left wall
        if ball.x < -maxpos:
            ball.velocity.x = -ball.velocity.x
            ball.x=-2*maxpos-ball.x
        # roof
        if ball.y > maxpos:
            ball.velocity.y = -ball.velocity.y
            ball.y=2*maxpos-ball.y
        #floor
        if ball.y < -maxpos:
            ball.velocity.y = -ball.velocity.y
            ball.y=-2*maxpos-ball.y
        #back wall
        if ball.z > maxpos:
            ball.velocity.z = -ball.velocity.z
            ball.z=2*maxpos-ball.z
        #front wall
        if ball.z < -maxpos:
            ball.velocity.z = -ball.velocity.z
            ball.z=-2*maxpos-ball.z

    ######################################
    # Ball Collision Detection
    ######################################
    #loop through all pairs
    for i in range(no_particles):
        for j in range(i+1,no_particles):
            distance=mag(ball_list[i].pos-ball_list[j].pos)
            #check collision
            if distance<(ball_list[i].radius+ball_list[j].radius):
                #unit vector in collision direction
                direction=norm(ball_list[j].pos-ball_list[i].pos)
                vi=dot(ball_list[i].velocity,direction)
                vj=dot(ball_list[j].velocity,direction)
                #impact velocity
                exchange=vj-vi
                #exchange momentum
                ball_list[i].velocity=ball_list[i].velocity + exchange*direction
                ball_list[j].velocity=ball_list[j].velocity - exchange*direction
                #adjust position
                overlap=2*ball_radius-distance
                ball_list[i].pos=ball_list[i].pos - overlap*direction
                ball_list[j].pos=ball_list[j].pos + overlap*direction

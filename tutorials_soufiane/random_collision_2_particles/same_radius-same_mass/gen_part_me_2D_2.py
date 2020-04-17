import numpy as np
from math import *
import random
import jdd_template as jdd
import re



CPU_I=1
CPU_J=1

Xmin = 0.0
Xmax = 1.0

Ymin = 0.0
Ymax = 1.0


nx = 2
ny = 1

rc = 0.02

NbParticles= nx*ny


Tstop=200.0


VAR_SIZE = 23


Imax=3*CPU_I
Jmax=3*CPU_J


INTG_METH='ADAMS_BASHFORTH'


mfixpdat=jdd.Template
mfixpdat=re.sub("CPU_I",str(CPU_I),mfixpdat)
mfixpdat=re.sub("CPU_J",str(CPU_J),mfixpdat)
mfixpdat=re.sub("in_imax",str(Imax),mfixpdat)
mfixpdat=re.sub("in_jmax",str(Jmax),mfixpdat)
mfixpdat=re.sub("in_xmin",str(Xmin),mfixpdat)
mfixpdat=re.sub("in_ymin",str(Ymin),mfixpdat)
mfixpdat=re.sub("in_xmax",str(Xmax),mfixpdat)
mfixpdat=re.sub("in_ymax",str(Ymax),mfixpdat)
mfixpdat=re.sub("NB_PARTICLES",str(NbParticles),mfixpdat)
mfixpdat=re.sub("intg_meth",str(INTG_METH),mfixpdat)
mfixpdat=re.sub("in_tstop",str(Tstop),mfixpdat)
mfixpdat=re.sub("var_size",str(VAR_SIZE),mfixpdat)
mfixpdat=re.sub("IN_DIAM",str(2*rc),mfixpdat)


particlepdat=""" """


sigma = 0.1
mu = 0.
c = 0.5

hx = 1./nx
hy = 1./ny

m1 = 3.26725645065308 # 2D
#m1 = 4.356341934204102E-002 # 3D
m2 = m1 

# -------------------------------------------------------- #

#-- random trajectory & intersection --#

#intersection_i = float(random.uniform(0,1))
#intersection_j = float(random.uniform(0,1))


# for i in list(range(nx)):
#  for j in list(range(ny)):
#   xc = random.uniform(0,1)
#   yc = random.uniform(0,1)
#   uc = intersection_i - xc
#   vc = intersection_j - yc

#   line="%f %f %5f %f %f %f \n"%(xc,yc,rc,2600.,uc,vc)
#   particlepdat = particlepdat+line

#xc1 = float(random.uniform(0,1))
#yc1 = float(random.uniform(0,1))
#xc2 = float(random.uniform(0,1))
#yc2 = float(random.uniform(0,1))
#uc1 = intersection_i - xc1
#vc1 = intersection_j - yc1
#uc2 = intersection_i - xc2
#vc2 = intersection_j - yc2

#line="%.12f %.12f %.12f %.12f %.12f %.12f \n"%(xc1,yc1,rc,2600.,uc1,vc1)
#particlepdat = particlepdat+line  
#line="%.12f %.12f %.12f %.12f %.12f %.12f \n"%(xc2,yc2,rc,2600.,uc2,vc2)
#particlepdat = particlepdat+line

# -------------------------------------------------------- #

#-- frontal collision --#

xc1 = float(random.uniform(0,1))
yc1 = float(random.uniform(0,1))

#-- diagonal collision --#

# xc2 = float(random.uniform(0,1))
# yc2 = float(random.uniform(0,1))

# if xc1 < xc2:
#  if yc1 < yc2:
#   uc1 = float(c*(xc2 - xc1))
#   vc1 = float(c*(yc2 - yc1))
#   uc2 = -uc1
#   vc2 = -vc1
  
#  elif yc1 > yc2:
#   uc1 = float(c*(xc2 - xc1))
#   vc1 = float(c*(yc2 - yc1))
#   uc2 = -uc1
#   vc2 = -vc1
  
# elif xc1 > xc2:
#  if yc1 < yc2:
#   uc1 = float(c*(xc2 - xc1))
#   vc1 = float(c*(yc2 - yc1))
#   uc2 = -uc1
#   vc2 = -vc1
  
#  elif yc1 > yc2:
#   uc1 = float(c*(xc2 - xc1))
#   vc1 = float(c*(yc2 - yc1))
#   uc2 = -uc1
#   vc2 = -vc1
  
#-- vertical collision --#
# xc2 = xc1
# yc2 = 1. - yc1  

# if yc1 < 0.5:
#  uc1 = 0.
#  #vc1 = sigma * random.randrange(1,10)
#  vc1 = float(random.uniform(0,1))   
# elif yc1 > 0.5:
#  uc1 = 0.
#  #vc1 = -sigma * random.randrange(1,10)
#  vc1 = -float(random.uniform(0,1))   
# uc2 = uc1
# vc2 = -vc1
 
#-- horizontal collision --#
xc2 = 1. - xc1
yc2 = yc1  

if xc1 < 0.5:
 uc1 = float(random.uniform(0,1))   
 # uc1 = sigma * random.randrange(1,10)
 vc1 = 0.
elif xc1 > 0.5:
 uc1 = -float(random.uniform(0,1))      
 # uc1 = -sigma * random.randrange(1,10)
 vc1 = 0.

uc2 = -uc1
vc2 = vc1
 
 
line="%.12f %.12f %.12f %.12f %.12f %.12f \n"%(xc1,yc1,rc,2600.,uc1,vc1)
particlepdat = particlepdat+line
line="%.12f %.12f %.12f %.12f %.12f %.12f \n"%(xc2,yc2,rc,2600.,uc2,vc2)  
particlepdat = particlepdat+line

# -------------------------------------------------------- #


analyticalpdat=""" """

# Vecteur  -> V, I, J
# Scalaire -> v
# Norme -> norm_V

I = np.array([1,0])
J = np.array([0,1])

mu_eq = ( m1 * m2 ) / ( m1 + m2 )

#######################################
v1x = uc1
v1y = vc1

# -------------------- #
V1 = v1x * I + v1y * J 
# -------------------- #

v2x = uc2
v2y = vc2

# -------------------- #
V2 = v2x * I + v2y * J 
# -------------------- #
#######################################


#######################################
# vG = ...
vGx = (m1 * v1x + m2 * v2x) / (m1 + m2)
vGy = (m1 * v1y + m2 * v2y) / (m1 + m2)

# -------------------- #
VG = vGx * I + vGy * J
# -------------------- #

#VG_p = VG
# -------------------- #
VG_p = VG
# -------------------- #
#######################################


#######################################
# v_star = v - vG
v1_star_x = v1x - vGx
v1_star_y = v1y - vGy

# -------------------- #
V1_star = v1_star_x * I + v1_star_y * J
# -------------------- #

v2_star_x = v2x - vGx
v2_star_y = v2y - vGy

# -------------------- #
V2_star = v2_star_x * I + v2_star_y * J
# -------------------- #
#######################################


#######################################
# norme (v_star_p) = norme (v_star)
V1_star_p_norm = sqrt( (v1_star_x)**2 + (v1_star_y)**2 )
V2_star_p_norm = sqrt( (v2_star_x)**2 + (v2_star_y)**2 )
#######################################


# -------------------- #
teta_star = -(atan2(v1_star_y,-(v1_star_x)))
# -------------------- #



#######################################
# vecteur unitaire U1_star_p
u1_star_x_p = cos(teta_star)
u1_star_y_p = sin(teta_star)

# -------------------- #
U1_star_p = u1_star_x_p * I + u1_star_y_p * J
# -------------------- #
#######################################


###################################################
# # teta = angle(VG,V1_star_p)
# v1_star_x_p = cos(teta_star) * u1_star_x_p
# v1_star_y_p = -sin(teta_star) * u1_star_y_p

# # -------------------- #
# V1_star_p = (v1_star_x_p * I) + (v1_star_y_p * J)
# # -------------------- #




# # V1_p = V1_star_p + VG
# v1_x_p = v1_star_x_p + vGx
# v1_y_p = v1_star_y_p + vGy

# # -------------------- #
# V1_p = v1_x_p * I + v1_y_p * J
# # -------------------- #
###################################################


###################################################
# W = V2 - V1
wx = v2x - v1x
wy = v2y - v1y

# -------------------- #
W = wx * I + wy * J
# -------------------- #

# -------------------- #
W_norm = sqrt(wx**2 + wy**2)
# -------------------- #

# W_p_norm = W_norm

# -------------------- #
W_p_norm = W_norm
# -------------------- #
###################################################


###################################################
# V1_star_p = ( (-mu_eq * W_p_norm) / m1 ) * U1_star_p
v1_star_x_p = ( (mu_eq * W_p_norm) / m1 ) * u1_star_x_p
v1_star_y_p = ( (mu_eq * W_p_norm) / m1 ) * u1_star_y_p

# -------------------- #
V1_star_p = v1_star_x_p * I + v1_star_y_p * J
# -------------------- #

# V1_p = V1_star_p + VG
v1_x_p = v1_star_x_p + vGx
v1_y_p = v1_star_y_p + vGy

# -------------------- #
V1_p = v1_x_p * I + v1_y_p * J
# -------------------- #
###################################################


###################################################
# V2_star_p = ( (-mu_eq * W_p_norm) / m2 ) * U1_star_p
v2_star_x_p = ( (-mu_eq * W_p_norm) / m2 ) * u1_star_x_p
v2_star_y_p = ( (-mu_eq * W_p_norm) / m2 ) * u1_star_y_p

# -------------------- #
V2_star_p = v2_star_x_p * I + v2_star_y_p * J
# -------------------- #

# V2_p = V2_star_p + VG
v2_x_p = v2_star_x_p + vGx
v2_y_p = v2_star_y_p + vGy

# -------------------- #
V2_p = v2_x_p * I + v2_y_p * J
# -------------------- #
###################################################



line="%.12f %.12f \n"%(v1_x_p,v1_y_p)
analyticalpdat = analyticalpdat+line
line="%.12f %.12f \n"%(v2_x_p,v2_y_p)
analyticalpdat = analyticalpdat+line



#mfix_file = open('mfix.dat', 'w')
#mfix_file.write(mfixpdat)
#mfix_file.close()

particle_file = open('particle_input.dat', 'w')
particle_file.write(particlepdat)
particle_file.close()

analytical_file = open('analytical_solution.dat', 'w')
analytical_file.write(analyticalpdat)
analytical_file.close()

exit()

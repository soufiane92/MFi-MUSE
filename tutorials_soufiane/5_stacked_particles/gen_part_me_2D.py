import numpy as np
import random 
import jdd_template as jdd
import re



CPU_I=1
CPU_J=1

Xmin = 0.0
Xmax = 1.0

Ymin = 0.0
Ymax = 1.0

#carre
nx = 10
ny = 10

#bocal
#nx = 10
#ny = 10


rc=0.02

# sigma=0.1
# mu=0

NbParticles= nx*ny

Tstop=200.0


VAR_SIZE = 21


Imax=3*CPU_I
Jmax=3*CPU_J

# cyclicX = True
# cyclicY = True

# i = 0



# des_en_in = 0.9

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

# mfixpdat=re.sub("cyclic_x",str(cyclicX),mfixpdat)
# mfixpdat=re.sub("cyclic_y",str(cyclicY),mfixpdat)


# mmax = 1
# mfixpdat=re.sub("M_MAX",str(mmax),mfixpdat)

# ic_ep_g = 0.1
# mfixpdat=re.sub("ic_ep_G",str(ic_ep_g),mfixpdat)

# nmaxg = 0
# mfixpdat=re.sub("nmaxG",str(nmaxg),mfixpdat)

# nmaxs = 0
# mfixpdat=re.sub("nmax_S",str(nmaxs),mfixpdat)

# ic_ep_s1 = 1 - ic_ep_g
# ic_ep_s2 = 1 - ic_ep_s1 - ic_ep_g
# ic_ep_s3 = 1 - ic_ep_s2

# if mmax == 1:
#  mfixpdat=re.sub("deseninput1",str(des_en_in),mfixpdat)
 
#  mfixpdat=re.sub("IN_DIAM",str(2*rc),mfixpdat)
#  mfixpdat=re.sub("ic_ep_S1",str(ic_ep_s1),mfixpdat)
#  uc = sigma * (np.random.randn()) + mu
#  vc = sigma * (np.random.randn()) + mu
#  mfixpdat=re.sub("ic_u_s1",str(uc),mfixpdat)
#  mfixpdat=re.sub("ic_v_s1",str(vc),mfixpdat)

 
# elif mmax == 2:
#  mfixpdat=re.sub("deseninput2",str(des_en_in),mfixpdat)

 
#  while i < int(NbParticles / 2):
#   mfixpdat=re.sub("IN_DIAMS2",str(rc),mfixpdat)

#  while i >= int(NbParticles / 2):
#   mfixpdat=re.sub("IN_DIAM",str(2*rc),mfixpdat)


  
particlepdat=""" """

sigma=0.1
mu=0

#carre
hx = 0.4/nx
hy = 0.4/ny

#bocal
#hx = 0.85/nx
#hy = 0.50/ny

for i in list(range(nx)):
 for j in list(range(ny)):
  xc = hx * (i)
  yc = hy * (j)
  # uc = sigma * np.random.randn() + mu
  # if (i % 2) == 0:
  #     uc=-0.1
  # else:
  #     uc=+0.1
      
  # if (j % 2) == 0:
  #     vc=-0.1
  # else:
  #     vc=+0.1
#  vc=0
  uc = sigma * (np.random.randn()) + mu
  vc = sigma * (np.random.randn()) + mu
  #vc=0
  #uc=0
#  vc=0
  #if (j % 2) == 0:
   #uc = 0.05
   #vc = 0
  #else:
   #uc = -0.05
   #vc = 0
 
  #carre
  line="%f %f %5f %f %f %f \n"%(xc+0.3,yc+0.3,rc,2600.,uc,vc)
  #bocal
  #line="%f %f %5f %f %f %f \n"%(xc+0.15,yc+0.5,rc,2600.,uc,vc)
  particlepdat = particlepdat+line



#mfix_file = open('mfix.dat', 'w')
#mfix_file.write(mfixpdat)
#mfix_file.close()

particle_file = open('particle_input.dat', 'w')
particle_file.write(particlepdat)
particle_file.close()


exit()

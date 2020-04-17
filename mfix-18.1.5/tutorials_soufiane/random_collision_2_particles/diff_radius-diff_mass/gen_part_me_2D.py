import numpy as np
import jdd_template as jdd
import re



CPU_I=4
CPU_J=4

Xmin = 0.0
Xmax = 1.0

Ymin = 0.0
Ymax = 1.0

nx = 20
ny = 20

NbParticles= nx*ny

Tstop=200.0


VAR_SIZE = 21


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

particlepdat=""" """

sigma=0.1
mu=0

hx = 1./nx
hy = 1./ny


rc=0.02

for i in list(range(nx)):
 for j in list(range(ny)):
  xc = hx * (i)
  yc = hy * (j)
  uc = sigma * np.random.randn() + mu
  if (i % 2) == 0:
      uc=-0.1
  else:
      uc=+0.1
      
  if (j % 2) == 0:
      vc=-0.1
  else:
      vc=+0.1
#  vc=0
  uc = sigma * (np.random.randn()) + mu
  vc = sigma * (np.random.randn()) + mu
#  vc=0
#  uc=0
#  vc=0
  #if (j % 2) == 0:
   #uc = 0.05
   #vc = 0
  #else:
   #uc = -0.05
   #vc = 0
  line="%f %f %5f %f %f %f \n"%(xc,yc,rc,2600.,uc,vc)
  particlepdat = particlepdat+line

mfixpdat=re.sub("IN_DIAM",str(2*rc),mfixpdat)
#print(mfixpdat)




mfix_file = open('mfix.dat', 'w')
mfix_file.write(mfixpdat)
mfix_file.close()

particle_file = open('particle_input.dat', 'w')
particle_file.write(particlepdat)
particle_file.close()


exit()

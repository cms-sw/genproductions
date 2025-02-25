
# For python linking (require f2py part of numpy)
ifeq ($(origin MENUM),undefined)
  MENUM=2
endif
P0_z_uux/matrix$(MENUM)py.so:
	cd P0_z_uux;make matrix$(MENUM)py.so
P0_z_ddx/matrix$(MENUM)py.so:
	cd P0_z_ddx;make matrix$(MENUM)py.so
all:  P0_z_uux/matrix$(MENUM)py.so  P0_z_ddx/matrix$(MENUM)py.so 
	echo 'done'
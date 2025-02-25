
# For python linking (require f2py part of numpy)
ifeq ($(origin MENUM),undefined)
  MENUM=2
endif
P0_wp_udx/matrix$(MENUM)py.so:
	cd P0_wp_udx;make matrix$(MENUM)py.so
P1_wm_dux/matrix$(MENUM)py.so:
	cd P1_wm_dux;make matrix$(MENUM)py.so
all:  P0_wp_udx/matrix$(MENUM)py.so  P1_wm_dux/matrix$(MENUM)py.so 
	echo 'done'
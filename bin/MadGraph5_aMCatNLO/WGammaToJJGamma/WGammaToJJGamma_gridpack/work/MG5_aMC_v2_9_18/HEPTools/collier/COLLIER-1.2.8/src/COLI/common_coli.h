!!
!!  File common_coli.h is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!* -*-Fortran-*-

!***********************************************************************
!*     file common_coli.h                                              *
!*     contains global common blocks for COLI                          *
!*---------------------------------------------------------------------*
!*     04.08.08  Ansgar Denner     last changed  27.03.15              *
!***********************************************************************


!c information output switch
      logical   coliinfo
      common/info_coli/coliinfo

#ifdef SING
!c regularization parameters
      real*8    deltauv,delta2ir,delta1ir,colishiftms2
      common/sing_coli/deltauv,delta2ir,delta1ir,colishiftms2
#endif
!c regularization parameters
      logical   ir_rat_terms
      real*8    muuv2,muir2
      common/dimreg_coli/muuv2,muir2,ir_rat_terms

!c infinitesimal masses
      integer    ncoliminf
      common /ncoliminf/    ncoliminf
      complex*16 coliminf(100)
      common /coliminf/     coliminf    
      complex*16 coliminf2(100)
      common /coliminf2/    coliminf2    
      complex*16 coliminffix(100)
      common /coliminffix/  coliminffix    
      complex*16 coliminffix2(100)
      common /coliminffix2/ coliminffix2    

!c scaling of infinitesimal masses
      real*8     coliminfscale,coliminfscale2
      common /colimsing/ coliminfscale,coliminfscale2

!c#ifdef ADcode
      character*16 masterfname
      integer      masterrank
      complex*16   masterargs(21)
      common /masterpar/ masterargs,masterfname,masterrank
!c#endif

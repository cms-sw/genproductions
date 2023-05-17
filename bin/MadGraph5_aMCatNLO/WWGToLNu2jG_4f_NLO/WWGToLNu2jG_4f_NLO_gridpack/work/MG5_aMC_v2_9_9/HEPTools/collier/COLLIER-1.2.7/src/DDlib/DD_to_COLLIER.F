!!
!!  File DD_to_COLLIER.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   
!!  Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, 
!!  see COPYING for details.
!!

**********************************************************************
        subroutine DDgetglobal(n,r,r2,r3,r4,r5,r6)
**********************************************************************
*       get global DD parameters from module DD_global
*       and pass them to COLLIER
*---------------------------------------------------------------------
*       27.2.2013 Lars Hofer
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer n,r,r2,r3,r4,r5,r6

        n  = nmax
        r  = rmax
        r2 = rmax2
        r3 = rmax3
!        r4 = rmax4
        r4 = r2max4
        r5 = rmax5
        r6 = rmax6

        end


**********************************************************************
        subroutine GetAccFlag_dd(val)
**********************************************************************
*       read accuracy information from DD and 
*       set accurcay flag of Collier
*---------------------------------------------------------------------
*       24.11.2014 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer val

        val=0

        if (accflag.eq.1) val = -1
        if (errflag.eq.1) val = -2

        end
**********************************************************************
        subroutine GetErrFlag_dd(val)
**********************************************************************
*       read error information from DD and 
*       set error flag of Collier
*---------------------------------------------------------------------
*       24.11.2014 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer val

        val = stopflag

        end
**********************************************************************
        subroutine SetErrFlag_dd(val)
**********************************************************************
*       set error flag of DD
*---------------------------------------------------------------------
*       07.07.2017 Ansgar Denner
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer val

        stopflag = val

        end

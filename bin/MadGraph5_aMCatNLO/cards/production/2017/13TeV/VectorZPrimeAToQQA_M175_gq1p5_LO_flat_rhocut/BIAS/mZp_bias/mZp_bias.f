C ************************************************************
C Source for the library implementing a bias function that 
C populates the large pt tale of the leading jet. 
C
C The two options of this subroutine, that can be set in
C the run card are:
C    > (double precision) ptj_bias_target_ptj : target ptj value
C    > (double precision) ptj_bias_enhancement_power : exponent
C
C Schematically, the functional form of the enhancement is
C    bias_wgt = [ptj(evt)/mean_ptj]^enhancement_power
C ************************************************************
C
C The following lines are read by MG5aMC to set what are the 
C relevant parameters for this bias module.
C
C  parameters = {}
C

C ************************************************************
C Helpers
C ************************************************************

      DOUBLE PRECISION FUNCTION negcb_Z(im)
        implicit none
        double precision im
        double precision ialpha
        double precision ixbar
        double precision isigma
        double precision iquadr
        double precision negm
        double precision dm
        double precision AA
        double precision BB

C      Fit mass spectrum with normal errors. Misses the low end by a bit.
C        ialpha =  0.428533
C        ixbar  = -209.945
C        isigma =  51.7438
C        iquadr =  0.000160548

C      Fit mass spectrum with reduced errors at the low end. Better low end, worse shoulder.
        ialpha =  0.368106
        ixbar = -231.695
        isigma =  59.7796
        iquadr = -0.000328664

        negm = -1. * im + (iquadr * im * im)
        dm = (negm - ixbar) / isigma
        IF (dm .GT. -1. * ialpha) THEN
          negcb_Z = 10000.0 * EXP(-0.5 * dm**2)
        ELSE
          negcb_Z = 10000.0 * EXP((ialpha**2 / 2.) + ABS(ialpha)*dm)
        ENDIF
      END

      DOUBLE PRECISION FUNCTION expo_y(ipt)
        implicit none
        double precision ipt
        double precision p0
        double precision p1
        double precision p2

        p0 = 1000.0
        p1 = 0.762182
        p2 = 0.397653
        expo_y = p0 * EXP(-1. * (ipt / p1)**p2)
      END

      subroutine bias_wgt(p, original_weight, bias_weight)
          implicit none
C
C Parameters
C
          include '../../maxparticles.inc'         
          include '../../nexternal.inc'
C
C Accessing the details of the event
C
          include '../../run_config.inc'
          include '../../lhe_event_infos.inc'
C
C Event kinematics
C
          double precision p(0:3,nexternal)
          double precision original_weight, bias_weight
          double precision rho
          double precision mZp
          double precision pTZp
          double precision pTy

C Cut variables, from run card          
          double precision p4(4)
          integer i
          integer j
          integer k
          double precision negcb_Z
          double precision negcb_y
          double precision negcb_weight
          double precision expo_y
          double precision expo_y_weight
C
C Global variables
C
C
C Mandatory common block to be defined in bias modules
C
          double precision stored_bias_weight
          data stored_bias_weight/1.0d0/          
          logical impact_xsec, requires_full_event_info
C         Don't unweight the bias: we really want a flat distribution
          data impact_xsec/.True./
C         Of course this module does not require the full event
C         information (color, resonances, helicities, etc..)
          data requires_full_event_info/.True./ 
          common/bias/stored_bias_weight,impact_xsec,
     &                requires_full_event_info
C
C Accessingt the details of the event
C
C --------------------
C BEGIN IMPLEMENTATION
C --------------------
          include '../../run.inc'
          include '../../cuts.inc'

          include '../bias.inc'

          p4(1) = 0.
          p4(2) = 0.
          p4(3) = 0.
          p4(4) = 0.
          mZp = -1.
          pTZp = -1.

          DO i=1,npart
            IF (jpart(1,i) .eq. 55) THEN
              p4(1) = pb(1,i)
              p4(2) = pb(2,i)
              p4(3) = pb(3,i)
              p4(4) = pb(0,i)
              mZp = sqrt(p4(4)**2 - p4(1)**2 - p4(2)**2 - p4(3)**2)
              pTZp = sqrt(p4(1)**2 + p4(2)**2)
            ELSE IF (jpart(1,i) .EQ. 22) THEN
              p4(1) = pb(1,i)
              p4(2) = pb(2,i)
              p4(3) = pb(3,i)
              p4(4) = pb(0,i)
              pTy = MAX(sqrt(p4(1)**2 + p4(2)**2), pTy)
            ENDIF
          ENDDO

C Rough cuts on mass, rho, and pT
          IF ((mZp .GT. 0.) .AND. (pTZp .GT. 0.)) THEN
            rho = LOG(mZp**2 / pTZp**2)
            IF (
     &         ((-8.0 .LT. rho) .AND. (rho .LT. -0.5)) .AND.
     &         ((10 .LT. pTZp) .AND. (pTZp .LT. 1500.)) .AND.
     &         ((10 .LT. mZp) .AND. (mZp .LT. 400.))) THEN

              negcb_weight = negcb_Z(mZp)
              expo_y_weight = expo_y(pTy)
              IF ((negcb_weight .GT. 0) .AND. (expo_y_weight .GT. 0)) THEN
                bias_weight = 1. / (negcb_weight * expo_y_weight)
              ELSE
                bias_weight = 0.00000001
              ENDIF
            ELSE
              bias_weight = 0.00000001
            ENDIF
          ELSE
C            print*,"jkl;"
            bias_weight = 0.00000001
          ENDIF
          RETURN
      END subroutine bias_wgt



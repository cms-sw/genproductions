C ************************************************************
C Source for the library implementing a bias function that 
C populates the mll tale of the mll. 
C
C The two options of this subroutine, that can be set in
C the run card are:
C    > (double precision) mll_bias_target_mll : target mll value
C    > (double precision) mll_bias_enhancement_power : exponent
C
C Schematically, the functional form of the enhancement is
C    bias_wgt = [mll(evt)/mean_mll]^enhancement_power
C ************************************************************
C
C The following lines are read by MG5aMC to set what are the 
C relevant parameters for this bias module.
C
C  parameters = {'mll_bias_target_mll': 1000.0,
C               'mll_bias_enhancement_power': 4.0}
C

      subroutine bias_wgt(p, original_weight, bias_weight)
          implicit none
C
C Parameters
C
          include '../../maxparticles.inc'          
          include '../../nexternal.inc'
C
C Variables
C
          double precision p(0:3,nexternal),sump(0:3),mass
          double precision original_weight, bias_weight
          integer i
          logical do_mass

c
c local variables defined in the run_card
c
          double precision mll_bias_target_mll
          double precision mll_bias_enhancement_power
C
C Global variables
C
C
C Mandatory common block to be defined in bias modules
C
          double precision stored_bias_weight
          data stored_bias_weight/1.0d0/          
          logical impact_xsec, requires_full_event_info
C         We only want to bias distributions, but not impact the xsec. 
          data impact_xsec/.False./
C         Of course this module does not require the full event
C         information (color, resonances, helicities, etc..)
          data requires_full_event_info/.False./ 
          common/bias/stored_bias_weight,impact_xsec,
     &                requires_full_event_info

C
C Accessingt the details of the event
C
          logical is_a_j(nexternal),is_a_l(nexternal),
     &            is_a_b(nexternal),is_a_a(nexternal),
     &            is_a_onium(nexternal),is_a_nu(nexternal),
     &            is_heavy(nexternal),do_cuts(nexternal)
          common/to_specisa/is_a_j,is_a_a,is_a_l,is_a_b,is_a_nu,
     &                      is_heavy,is_a_onium,do_cuts

C
C    Setup the value of the parameters from the run_card    
C
      include '../bias.inc'

C --------------------
C BEGIN IMPLEMENTATION
C --------------------

      sump(0)=0d0
      sump(1)=0d0
      sump(2)=0d0
      sump(3)=0d0
      do_mass=.false.
      do i=1,nexternal
         if (is_a_l(i)) then
            do_mass=.true.
            sump(0)=sump(0)+p(0,i)
            sump(1)=sump(1)+p(1,i)
            sump(2)=sump(2)+p(2,i)
            sump(3)=sump(3)+p(3,i)
         endif
      enddo
      if (.not.do_mass) then 
        bias_weight=1d0
        return
      endif
      mass = sqrt(sump(0)**2 - sump(1)**2 - sump(2)**2 - sump(3)**2)
      bias_weight=(mass/mll_bias_target_mll)**mll_bias_enhancement_power

      return

      end subroutine bias_wgt

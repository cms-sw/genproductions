      subroutine rwl_setup_params_weights(count)
      implicit none
      integer count
      integer, save :: old_pdf1,old_pdf2
      real * 8, save :: old_facfact,old_renfact
      real * 8, save :: old_sthw2,old_Zmass,old_Wmass,old_Zwidth,old_Wwidth
      include 'pwhg_rwl.h'
      include 'pwhg_pdf.h'
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'PhysPars.h'
      logical rwl_keypresent
      real * 8 val
      character * 5 scheme
      integer iorder,iret
      real * 8 dummy
      call setlocalscales(0,-1,dummy)
      if(count==0) then
         old_pdf1=pdf_ndns1
         old_pdf2=pdf_ndns2
         old_facfact = st_facfact
         old_renfact = st_renfact
         old_sthw2 = ph_sthw2
         old_Zmass = ph_Zmass
         old_Wmass = ph_Wmass
         old_Zwidth = ph_Zwidth
         old_Wwidth = ph_Wwidth
      elseif(count == -1) then
         pdf_ndns1 = old_pdf1
         pdf_ndns2 = old_pdf2
         st_facfact = old_facfact
         st_renfact = old_renfact
         
         ph_sthw2 = old_sthw2
         ph_sthw = sqrt(ph_sthw2)
         ph_cthw = sqrt(1-ph_sthw2)
         
         ph_Zmass = old_Zmass
         ph_Zmass2 = ph_Zmass**2
         ph_Zwidth = old_Zwidth
         ph_ZmZw = ph_Zmass * ph_Zwidth
         
         ph_Wmass = old_Wmass
         ph_Wmass2 = ph_Wmass**2
         ph_Wwidth = old_Wwidth
         ph_WmWw = ph_Wmass * ph_Wwidth
         
         call genericpdfpar(pdf_ndns1,pdf_ih1,st_lambda5MSB,
     1        scheme,iorder,iret)
      else
         if(count > rwl_num_weights) then
            write(*,*) 'rwl_setup_params_weights: '
            write(*,*) 'error, count > rwl_num_weights, exiting ...'
            call exit(-1)
         endif
         if(rwl_keypresent(count,'lhapdf',val)) then
c for safety, we convert the real val to the nearest integer
            pdf_ndns1 = nint(val)
            pdf_ndns2 = pdf_ndns1
            call genericpdfpar(pdf_ndns1,pdf_ih1,st_lambda5MSB,
     1           scheme,iorder,iret)
         endif
         if(rwl_keypresent(count,'facscfact',val)) then
            st_facfact = val
         endif
         if(rwl_keypresent(count,'renscfact',val)) then
            st_renfact = val
         endif
         if(rwl_keypresent(count,'zmass',val)) then
            ph_Zmass = val
            ph_Zmass = ph_Zmass/sqrt(1.d0+(osZwidth/ph_Zmass)**2)
            
            ph_Zmass2 = ph_Zmass**2
            ph_ZmZw = ph_Zmass * ph_Zwidth
         endif
         if(rwl_keypresent(count,'wmass',val)) then
            ph_Wmass = val
            ph_Wmass = ph_Wmass/sqrt(1.d0+(osWwidth/ph_Wmass)**2)
            
            ph_Wmass2 = ph_Wmass**2
            ph_WmWw = ph_Wmass * ph_Wwidth
         endif
         ph_sthw2 = abs(1d0-(ph_Wmass/ph_Zmass)**2)
         ph_sthw = sqrt(ph_sthw2)
         ph_cthw = sqrt(1-ph_sthw2)
         
         ph_unit_e = sqrt(ph_gmu * 8d0/sqrt(2d0) * ph_Wmass**2 * ph_sthw2)
         ph_alphaem = ph_unit_e*ph_unit_e/4d0/pi
      endif
      end


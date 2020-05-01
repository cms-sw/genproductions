      subroutine rwl_setup_params_weights(count)
      implicit none
      integer count
      integer, save :: old_pdf1,old_pdf2
      real * 8, save :: old_facfact,old_renfact
      include 'pwhg_rwl.h'
      include 'pwhg_pdf.h'
      include 'pwhg_st.h'
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
      elseif(count == -1) then
         pdf_ndns1 = old_pdf1
         pdf_ndns2 = old_pdf2
         st_facfact = old_facfact
         st_renfact = old_renfact
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
      endif
      end

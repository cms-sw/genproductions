
      integer maxnup 
      parameter (maxnup=500)
      integer nup,idprup,idup(maxnup),istup(maxnup),mothup(2,maxnup)
     & ,icolup(2,maxnup)
      double precision xwgtup,scalup,aqedup,aqcdup,pup(5,maxnup)
     & ,spinup(maxnup) ,vtimup(maxnup)
      common /hepeup/nup,idprup,idup,istup,mothup,icolup,
     &     xwgtup,scalup,aqedup,aqcdup,pup,vtimup
     & ,spinup 

auto V eps;
V p1,...,p6,k1,...,k6,Q,L1,...,L6;
S Mu2;

#ifdef `NINJAGEN_EXP_FEATURE'

L SixPhotons = - 64*L1.eps12*L2.eps23*L3.eps34*L4.eps45*L5.eps56*L6.eps61 - 64*
         L1.eps21*L2.eps32*L3.eps43*L4.eps54*L5.eps65*L6.eps16;

#else

L SixPhotons = - 64*k1.eps12*eps61.Q*eps23.Q*eps34.Q*eps45.Q*eps56.Q + 64*k1.eps12*
         eps61.Q*eps23.Q*eps34.Q*eps45.Q*eps56.k6 - 64*k1.eps12*eps61.Q*
         eps23.Q*eps34.Q*eps45.p4*eps56.Q + 64*k1.eps12*eps61.Q*eps23.Q*
         eps34.Q*eps45.p4*eps56.k6 - 64*k1.eps12*eps61.Q*eps23.Q*eps34.p3*
         eps45.Q*eps56.Q + 64*k1.eps12*eps61.Q*eps23.Q*eps34.p3*eps45.Q*
         eps56.k6 - 64*k1.eps12*eps61.Q*eps23.Q*eps34.p3*eps45.p4*eps56.Q + 64
         *k1.eps12*eps61.Q*eps23.Q*eps34.p3*eps45.p4*eps56.k6 - 64*k1.eps12*
         eps61.Q*eps23.p2*eps34.Q*eps45.Q*eps56.Q + 64*k1.eps12*eps61.Q*
         eps23.p2*eps34.Q*eps45.Q*eps56.k6 - 64*k1.eps12*eps61.Q*eps23.p2*
         eps34.Q*eps45.p4*eps56.Q + 64*k1.eps12*eps61.Q*eps23.p2*eps34.Q*
         eps45.p4*eps56.k6 - 64*k1.eps12*eps61.Q*eps23.p2*eps34.p3*eps45.Q*
         eps56.Q + 64*k1.eps12*eps61.Q*eps23.p2*eps34.p3*eps45.Q*eps56.k6 - 64
         *k1.eps12*eps61.Q*eps23.p2*eps34.p3*eps45.p4*eps56.Q + 64*k1.eps12*
         eps61.Q*eps23.p2*eps34.p3*eps45.p4*eps56.k6 - 64*k1.eps21*eps16.Q*
         eps32.Q*eps43.Q*eps54.Q*eps65.Q + 64*k1.eps21*eps16.Q*eps32.Q*eps43.Q
         *eps54.Q*eps65.k6 - 64*k1.eps21*eps16.Q*eps32.Q*eps43.Q*eps54.p4*
         eps65.Q + 64*k1.eps21*eps16.Q*eps32.Q*eps43.Q*eps54.p4*eps65.k6 - 64*
         k1.eps21*eps16.Q*eps32.Q*eps43.p3*eps54.Q*eps65.Q + 64*k1.eps21*
         eps16.Q*eps32.Q*eps43.p3*eps54.Q*eps65.k6 - 64*k1.eps21*eps16.Q*
         eps32.Q*eps43.p3*eps54.p4*eps65.Q + 64*k1.eps21*eps16.Q*eps32.Q*
         eps43.p3*eps54.p4*eps65.k6 - 64*k1.eps21*eps16.Q*eps32.p2*eps43.Q*
         eps54.Q*eps65.Q + 64*k1.eps21*eps16.Q*eps32.p2*eps43.Q*eps54.Q*
         eps65.k6 - 64*k1.eps21*eps16.Q*eps32.p2*eps43.Q*eps54.p4*eps65.Q + 64
         *k1.eps21*eps16.Q*eps32.p2*eps43.Q*eps54.p4*eps65.k6 - 64*k1.eps21*
         eps16.Q*eps32.p2*eps43.p3*eps54.Q*eps65.Q + 64*k1.eps21*eps16.Q*
         eps32.p2*eps43.p3*eps54.Q*eps65.k6 - 64*k1.eps21*eps16.Q*eps32.p2*
         eps43.p3*eps54.p4*eps65.Q + 64*k1.eps21*eps16.Q*eps32.p2*eps43.p3*
         eps54.p4*eps65.k6 - 64*eps12.Q*eps61.Q*eps23.Q*eps34.Q*eps45.Q*
         eps56.Q + 64*eps12.Q*eps61.Q*eps23.Q*eps34.Q*eps45.Q*eps56.k6 - 64*
         eps12.Q*eps61.Q*eps23.Q*eps34.Q*eps45.p4*eps56.Q + 64*eps12.Q*eps61.Q
         *eps23.Q*eps34.Q*eps45.p4*eps56.k6 - 64*eps12.Q*eps61.Q*eps23.Q*
         eps34.p3*eps45.Q*eps56.Q + 64*eps12.Q*eps61.Q*eps23.Q*eps34.p3*
         eps45.Q*eps56.k6 - 64*eps12.Q*eps61.Q*eps23.Q*eps34.p3*eps45.p4*
         eps56.Q + 64*eps12.Q*eps61.Q*eps23.Q*eps34.p3*eps45.p4*eps56.k6 - 64*
         eps12.Q*eps61.Q*eps23.p2*eps34.Q*eps45.Q*eps56.Q + 64*eps12.Q*eps61.Q
         *eps23.p2*eps34.Q*eps45.Q*eps56.k6 - 64*eps12.Q*eps61.Q*eps23.p2*
         eps34.Q*eps45.p4*eps56.Q + 64*eps12.Q*eps61.Q*eps23.p2*eps34.Q*
         eps45.p4*eps56.k6 - 64*eps12.Q*eps61.Q*eps23.p2*eps34.p3*eps45.Q*
         eps56.Q + 64*eps12.Q*eps61.Q*eps23.p2*eps34.p3*eps45.Q*eps56.k6 - 64*
         eps12.Q*eps61.Q*eps23.p2*eps34.p3*eps45.p4*eps56.Q + 64*eps12.Q*
         eps61.Q*eps23.p2*eps34.p3*eps45.p4*eps56.k6 - 64*eps21.Q*eps16.Q*
         eps32.Q*eps43.Q*eps54.Q*eps65.Q + 64*eps21.Q*eps16.Q*eps32.Q*eps43.Q*
         eps54.Q*eps65.k6 - 64*eps21.Q*eps16.Q*eps32.Q*eps43.Q*eps54.p4*
         eps65.Q + 64*eps21.Q*eps16.Q*eps32.Q*eps43.Q*eps54.p4*eps65.k6 - 64*
         eps21.Q*eps16.Q*eps32.Q*eps43.p3*eps54.Q*eps65.Q + 64*eps21.Q*eps16.Q
         *eps32.Q*eps43.p3*eps54.Q*eps65.k6 - 64*eps21.Q*eps16.Q*eps32.Q*
         eps43.p3*eps54.p4*eps65.Q + 64*eps21.Q*eps16.Q*eps32.Q*eps43.p3*
         eps54.p4*eps65.k6 - 64*eps21.Q*eps16.Q*eps32.p2*eps43.Q*eps54.Q*
         eps65.Q + 64*eps21.Q*eps16.Q*eps32.p2*eps43.Q*eps54.Q*eps65.k6 - 64*
         eps21.Q*eps16.Q*eps32.p2*eps43.Q*eps54.p4*eps65.Q + 64*eps21.Q*
         eps16.Q*eps32.p2*eps43.Q*eps54.p4*eps65.k6 - 64*eps21.Q*eps16.Q*
         eps32.p2*eps43.p3*eps54.Q*eps65.Q + 64*eps21.Q*eps16.Q*eps32.p2*
         eps43.p3*eps54.Q*eps65.k6 - 64*eps21.Q*eps16.Q*eps32.p2*eps43.p3*
         eps54.p4*eps65.Q + 64*eps21.Q*eps16.Q*eps32.p2*eps43.p3*eps54.p4*
         eps65.k6;

#endif

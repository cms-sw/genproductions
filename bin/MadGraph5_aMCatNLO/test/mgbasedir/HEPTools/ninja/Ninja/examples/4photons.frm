V k1,k2,k3,k4;
V p2;
Autodeclare symbols spae, spbe;
Autodeclare Vectors eps;
S [mf2_];

S Mu2;
V Q;

L FourPhotons = [mf2_] * (  - 4*k1.eps12*eps23.Q*spae34*spbe41 - 4*k1.eps12*eps23.p2*
         spae34*spbe41 - 4*k1.eps12*eps41.Q*spae23*spbe34 - 4*k1.eps12*eps43.Q
         *spae23*spbe41 + 4*k1.eps12*eps43.k4*spae23*spbe41 - 4*k1.eps21*
         eps34.Q*spae41*spbe23 + 4*k1.eps21*eps34.k4*spae41*spbe23 - 4*
         k1.eps21*eps32.Q*spae41*spbe34 - 4*k1.eps21*eps32.p2*spae41*spbe34 - 
         4*k1.eps21*eps14.Q*spae34*spbe23 - 4*eps12.Q*eps23.Q*spae34*spbe41 - 
         4*eps12.Q*eps23.p2*spae34*spbe41 - 4*eps12.Q*eps41.Q*spae23*spbe34 - 
         4*eps12.Q*eps43.Q*spae23*spbe41 + 4*eps12.Q*eps43.k4*spae23*spbe41 + 
         4*eps23.Q*eps34.Q*spae41*spbe21 - 4*eps23.Q*eps34.k4*spae41*spbe21 + 
         4*eps23.Q*eps14.Q*spae34*spbe21 + 4*eps23.p2*eps34.Q*spae41*spbe21 - 
         4*eps23.p2*eps34.k4*spae41*spbe21 + 4*eps23.p2*eps14.Q*spae34*spbe21
          + 4*eps34.Q*eps41.Q*spae21*spbe23 - 4*eps34.Q*eps21.Q*spae41*spbe23
          - 4*eps34.k4*eps41.Q*spae21*spbe23 + 4*eps34.k4*eps21.Q*spae41*
         spbe23 + 4*eps41.Q*eps32.Q*spae21*spbe34 + 4*eps41.Q*eps32.p2*spae21*
         spbe34 - 4*eps21.Q*eps32.Q*spae41*spbe34 - 4*eps21.Q*eps32.p2*spae41*
         spbe34 - 4*eps21.Q*eps14.Q*spae34*spbe23 + 4*eps32.Q*eps43.Q*spae21*
         spbe41 - 4*eps32.Q*eps43.k4*spae21*spbe41 + 4*eps32.p2*eps43.Q*spae21
         *spbe41 - 4*eps32.p2*eps43.k4*spae21*spbe41 + 4*eps43.Q*eps14.Q*
         spae23*spbe21 - 4*eps43.k4*eps14.Q*spae23*spbe21 )

       + [mf2_]^2 * ( spae34*spae21*spbe23*spbe41 + spae23*spae41*spbe34*
         spbe21 )

       + Mu2 * ( 4*k1.eps12*eps23.Q*spae34*spbe41 + 4*k1.eps12*eps23.p2*spae34
         *spbe41 + 4*k1.eps12*eps41.Q*spae23*spbe34 + 4*k1.eps12*eps43.Q*
         spae23*spbe41 - 4*k1.eps12*eps43.k4*spae23*spbe41 + 4*k1.eps21*
         eps34.Q*spae41*spbe23 - 4*k1.eps21*eps34.k4*spae41*spbe23 + 4*
         k1.eps21*eps32.Q*spae41*spbe34 + 4*k1.eps21*eps32.p2*spae41*spbe34 + 
         4*k1.eps21*eps14.Q*spae34*spbe23 + 4*eps12.Q*eps23.Q*spae34*spbe41 + 
         4*eps12.Q*eps23.p2*spae34*spbe41 + 4*eps12.Q*eps41.Q*spae23*spbe34 + 
         4*eps12.Q*eps43.Q*spae23*spbe41 - 4*eps12.Q*eps43.k4*spae23*spbe41 - 
         4*eps23.Q*eps34.Q*spae41*spbe21 + 4*eps23.Q*eps34.k4*spae41*spbe21 - 
         4*eps23.Q*eps14.Q*spae34*spbe21 - 4*eps23.p2*eps34.Q*spae41*spbe21 + 
         4*eps23.p2*eps34.k4*spae41*spbe21 - 4*eps23.p2*eps14.Q*spae34*spbe21
          - 4*eps34.Q*eps41.Q*spae21*spbe23 + 4*eps34.Q*eps21.Q*spae41*spbe23
          + 4*eps34.k4*eps41.Q*spae21*spbe23 - 4*eps34.k4*eps21.Q*spae41*
         spbe23 - 4*eps41.Q*eps32.Q*spae21*spbe34 - 4*eps41.Q*eps32.p2*spae21*
         spbe34 + 4*eps21.Q*eps32.Q*spae41*spbe34 + 4*eps21.Q*eps32.p2*spae41*
         spbe34 + 4*eps21.Q*eps14.Q*spae34*spbe23 - 4*eps32.Q*eps43.Q*spae21*
         spbe41 + 4*eps32.Q*eps43.k4*spae21*spbe41 - 4*eps32.p2*eps43.Q*spae21
         *spbe41 + 4*eps32.p2*eps43.k4*spae21*spbe41 - 4*eps43.Q*eps14.Q*
         spae23*spbe21 + 4*eps43.k4*eps14.Q*spae23*spbe21 )

       + Mu2*[mf2_] * (  - 2*spae34*spae21*spbe23*spbe41 - 2*spae23*spae41*
         spbe34*spbe21 )

       + Mu2^2 * ( spae34*spae21*spbe23*spbe41 + spae23*spae41*spbe34*spbe21 )

       - 16*k1.eps12*eps23.Q*eps34.Q*eps41.Q + 16*k1.eps12*eps23.Q*eps34.k4*
         eps41.Q - 16*k1.eps12*eps23.p2*eps34.Q*eps41.Q + 16*k1.eps12*eps23.p2
         *eps34.k4*eps41.Q - 16*k1.eps21*eps32.Q*eps43.Q*eps14.Q + 16*k1.eps21
         *eps32.Q*eps43.k4*eps14.Q - 16*k1.eps21*eps32.p2*eps43.Q*eps14.Q + 16
         *k1.eps21*eps32.p2*eps43.k4*eps14.Q - 16*eps12.Q*eps23.Q*eps34.Q*
         eps41.Q + 16*eps12.Q*eps23.Q*eps34.k4*eps41.Q - 16*eps12.Q*eps23.p2*
         eps34.Q*eps41.Q + 16*eps12.Q*eps23.p2*eps34.k4*eps41.Q - 16*eps21.Q*
         eps32.Q*eps43.Q*eps14.Q + 16*eps21.Q*eps32.Q*eps43.k4*eps14.Q - 16*
         eps21.Q*eps32.p2*eps43.Q*eps14.Q + 16*eps21.Q*eps32.p2*eps43.k4*
         eps14.Q;
         ;



#-
Off Statistics;

#include `DIAGFILE'

.sort
Hide;
.sort

S ninjaT, ninjaTi, ninjaX;
S ninjaP, ninjaP0, ninjaP1, ninjaP2;
V ninjaA, ninjaA0, ninjaA1, ninjaE3, ninjaE4;
S ninjaMu2;
V ninjaQ;
#ifdef `LOOPPREFIX'
auto V ninjaAl, ninjaA0l, ninjaA1l; 
auto V `LOOPPREFIX';
#endif

L ninjaDiag = `DIAGNAME';

multiply, replace_(`QVAR',ninjaQ,`MU2VAR',ninjaMu2);

.sort
Hide;
.sort

L ninjaDiag3 = ninjaDiag;

#if `DIAGN' >= 3
multiply, ninjaTi^{`DIAGN'-3};
#else
multiply, ninjaT^{3-`DIAGN'};
#endif

id ninjaQ = ninjaA + ninjaE3*ninjaT + (ninjaP+ninjaMu2)*ninjaTi*ninjaE4;
#ifdef `LOOPPREFIX'
 #do i=0,`DIAGN' 
id `LOOPPREFIX'`i' = ninjaAl`i' + ninjaE3*ninjaT + (ninjaP+ninjaMu2)*ninjaTi*ninjaE4;
 #enddo
#endif

id ninjaT * ninjaTi = 1;
id ninjaTi = 0;

id ninjaE3.ninjaE3 = 0;
id ninjaE4.ninjaE4 = 0;
id ninjaE3.ninjaE4 = 1/2;

Bracket ninjaT;
.sort

L ninjaDiag31 = 0
#if `DIAGRANK' >= `DIAGN'
+ ninjaDiag3[ninjaT^4]*ninjaT^4
#endif
+ ninjaDiag3[ninjaT^3]*ninjaT^3
+ ninjaDiag3[ninjaT^2]*ninjaT^2;

#if `DIAGN' >= 3
L ninjaDiag32 = ninjaDiag3[ninjaT^1]*ninjaT^1 + ninjaDiag3[ninjaT^0];
#endif

.sort

#write <`OUTFILE'> "*--#[ ninjaDiag31:"
#write <`OUTFILE'> "L ninjaDiag31 = %E + NINJAZERO ;", ninjaDiag31;
#write <`OUTFILE'> "*--#] ninjaDiag31:"

#if `DIAGN' >= 3
#write <`OUTFILE'> "*--#[ ninjaDiag32:"
#write <`OUTFILE'> "L ninjaDiag32 = %E + NINJAZERO ;", ninjaDiag32;
#write <`OUTFILE'> "*--#] ninjaDiag32:"
#endif

.sort
Hide;
.sort

L ninjaDiag2 = ninjaDiag3;

multiply, ninjaTi;
id ninjaT * ninjaTi = 1;
id ninjaTi = 0;

id ninjaA = ninjaA0 + ninjaA1*ninjaX;
#ifdef `LOOPPREFIX'
 #do i=0,`DIAGN' 
id ninjaAl`i' =  ninjaA0l`i' + ninjaA1*ninjaX;
 #enddo
#endif
id ninjaP = ninjaP0 + ninjaP1*ninjaX + ninjaP2*ninjaX^2;
id ninjaA1.ninjaE3 = 0;
id ninjaA1.ninjaE4 = 0;

Bracket ninjaT;
.sort

L ninjaDiag21 = 0
#if `DIAGRANK' >= `DIAGN'
+ ninjaDiag2[ninjaT^3]*ninjaT^3
#endif
+ ninjaDiag2[ninjaT^2]*ninjaT^2
+ ninjaDiag2[ninjaT]*ninjaT;

L ninjaDiag22 = ninjaDiag2[ninjaT^0];

.sort

#write <`OUTFILE'> "*--#[ ninjaDiag21:"
#write <`OUTFILE'> "L ninjaDiag21 = %E + NINJAZERO ;", ninjaDiag21;
#write <`OUTFILE'> "*--#] ninjaDiag21:"

#write <`OUTFILE'> "*--#[ ninjaDiag22:"
#write <`OUTFILE'> "L ninjaDiag22 = %E + NINJAZERO ;", ninjaDiag22;
#write <`OUTFILE'> "*--#] ninjaDiag22:"

.sort

#if (`DIAGRANK' >= `DIAGN') && (`DIAGN' >= 4)

Hide;
.sort

L ninjaDiagMu2 = ninjaDiag * ninjaTi^{`DIAGN'};

id ninjaQ = ninjaA0 * ninjaT
#if (`DIAGRANK' > `DIAGN')
 + ninjaA1
#endif
;
#ifdef `LOOPPREFIX'
 #do i=0,`DIAGN' 
id `LOOPPREFIX'`i' = ninjaA0 * ninjaT
#if (`DIAGRANK' > `DIAGN')
 + ninjaA1l`i'
#endif
;
 #enddo
#endif
id ninjaMu2 = ninjaA0.ninjaA0 * ninjaT^2;
id ninjaT * ninjaTi = 1;
id ninjaTi = 0;

.sort

#write <`OUTFILE'> "*--#[ ninjaDiagMu2:"
#write <`OUTFILE'> "L ninjaDiagMu2 = %E + NINJAZERO ;", ninjaDiagMu2;
#write <`OUTFILE'> "*--#] ninjaDiagMu2:"

#endif

.end
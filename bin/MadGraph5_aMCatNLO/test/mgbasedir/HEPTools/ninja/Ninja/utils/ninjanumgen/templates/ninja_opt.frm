#-
Off Statistics;
S ninjaMu2;
V ninjaQ;
CF ninjaMP;
S ninjaT, ninjaTi, ninjaX;
S ninjaP, ninjaP0, ninjaP1, ninjaP2, NINJAZERO;
V ninjaA, ninjaA0, ninjaA1, ninjaE3, ninjaE4; 
#ifdef `LOOPPREFIX'
auto V ninjaAl, ninjaA0l, ninjaA1l; 
auto V `LOOPPREFIX';
#endif

#include `DIAGFILE'

#if `EXPANSIONID' == SAMURAINUM
* Optimize samurai-like numerator

multiply, replace_(`QVAR',ninjaQ,`MU2VAR',ninjaMu2);
id ninjaA0?.ninjaA1? = ninjaMP(ninjaA0,ninjaA1);
id 1/ninjaA0?.ninjaA1? = 1/ninjaMP(ninjaA0,ninjaA1);
.sort
ExtraSymbols,vector,ninjanumabbr;
Format O`OPTIMIZATIONLEVEL';
Format `LANGUAGE',nospaces,stats=off;
#optimize `DIAGNAME';
.sort

#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        NinjaAbbrType ninjanumabbr[`optimmaxvar_'];";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "%O";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "      ninja_result = ";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "          %e ;",`DIAGNAME'(ninja_result);

#else
* Optimize Laurent expansion

.sort
Hide;
.sort

#include `TEMPPREFIX'`EXPANSIONFILE' #ninjaDiag`EXPANSIONID'
id ninjaA0?.ninjaA1? = ninjaMP(ninjaA0,ninjaA1);
id 1/ninjaA0?.ninjaA1? = 1/ninjaMP(ninjaA0,ninjaA1);
Bracket ninjaT, ninjaX, ninjaMu2;
.sort

ExtraSymbols,vector,ninjanumabbr;
Keep Brackets;
Format O`OPTIMIZATIONLEVEL';
Format `LANGUAGE',nospaces,stats=off;
#optimize ninjaDiag`EXPANSIONID';
Bracket ninjaT, ninjaX, ninjaMu2;
.sort


* Write optimized expansion

#if `EXPANSIONID' == 31

#do tpow={3+`DIAGRANK'-`DIAGN'},2,-1
 #do mupow=0,{3+`DIAGRANK'-`DIAGN'}-`tpow',2
     Local ninjaDiagt`tpow'mu`mupow' = ninjaDiag`EXPANSIONID'[ninjaT^`tpow'*ninjaMu2^{`mupow'/2}];
 #enddo
#enddo
.sort
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        NinjaAbbrType ninjanumabbr[`optimmaxvar_'];";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "%O";
#do tpow={3+`DIAGRANK'-`DIAGN'},2,-1
 #do mupow=0,{3+`DIAGRANK'-`DIAGN'}-`tpow',2
	 #write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "      ninjaC[ninjaidxt`tpow'mu`mupow'] = ";
	 #write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        %e ;",ninjaDiagt`tpow'mu`mupow'(ninjaC[ninjaidxt`tpow'mu`mupow']);
 #enddo
#enddo

#endif


#if `EXPANSIONID' == 32

#do tpow=1,0,-1
	#do mupow=0,{3+`DIAGRANK'-`DIAGN'}-`tpow',2
		Local ninjaDiagt`tpow'mu`mupow' = ninjaDiag`EXPANSIONID'[ninjaT^`tpow'*ninjaMu2^{`mupow'/2}];
	#enddo
#enddo
.sort
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        NinjaAbbrType ninjanumabbr[`optimmaxvar_'];";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "%O";
#do tpow=1,0,-1
	#do mupow=0,{3+`DIAGRANK'-`DIAGN'}-`tpow',2
	 #write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "      ninjaC[ninjaidxt`tpow'mu`mupow'] = ";
	 #write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        %e ;",ninjaDiagt`tpow'mu`mupow'(ninjaC[ninjaidxt`tpow'mu`mupow']);
	#enddo
#enddo

#endif


#if `EXPANSIONID' == 21

#do tpow={2+`DIAGRANK'-`DIAGN'},1,-1
	#do xpow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'
		#do mupow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'-`xpow',2
			Local ninjaDiagt`tpow'x`xpow'mu`mupow' = ninjaDiag`EXPANSIONID'[ninjaT^`tpow'*ninjaX^`xpow'*ninjaMu2^{`mupow'/2}];
		#enddo
	#enddo
#enddo
.sort
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        NinjaAbbrType ninjanumabbr[`optimmaxvar_'];";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "%O";
#do tpow={2+`DIAGRANK'-`DIAGN'},1,-1
	#do xpow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'
		#do mupow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'-`xpow',2
			#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "      ninjaC[ninjaidxt`tpow'x`xpow'mu`mupow'] = ";
			#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        %e ;",ninjaDiagt`tpow'x`xpow'mu`mupow'(ninjaC[ninjaidxt`tpow'x`xpow'mu`mupow']);
		#enddo
	#enddo
#enddo

#endif


#if `EXPANSIONID' == 22

#do tpow=0,0,-1
	#do xpow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'
		#do mupow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'-`xpow',2
			Local ninjaDiagt`tpow'x`xpow'mu`mupow' = ninjaDiag`EXPANSIONID'[ninjaT^`tpow'*ninjaX^`xpow'*ninjaMu2^{`mupow'/2}];
		#enddo
	#enddo
#enddo
.sort
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        NinjaAbbrType ninjanumabbr[`optimmaxvar_'];";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "%O";
#do tpow=0,0,-1
	#do xpow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'
		#do mupow=0,{2+`DIAGRANK'-`DIAGN'}-`tpow'-`xpow',2
			#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "      ninjaC[ninjaidxt`tpow'x`xpow'mu`mupow'] = ";
			#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        %e ;",ninjaDiagt`tpow'x`xpow'mu`mupow'(ninjaC[ninjaidxt`tpow'x`xpow'mu`mupow']);
		#enddo
	#enddo
#enddo

#endif


#if `EXPANSIONID' == Mu2

#do tpow=`DIAGRANK'-`DIAGN',0,-1
	Local ninjaDiagt`tpow' = ninjaDiag`EXPANSIONID'[ninjaT^`tpow'];
#enddo
.sort
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        NinjaAbbrType ninjanumabbr[`optimmaxvar_'];";
#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "%O";
#do tpow=`DIAGRANK'-`DIAGN',0,-1
	#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "      ninjaC[ninjaidxt`tpow'] = ";
	#write <`TEMPPREFIX'ninja_opt`EXPANSIONID'.out> "        %e ;",ninjaDiagt`tpow'(ninjaC[ninjaidxt`tpow']);
#enddo

#endif


#endif

.end
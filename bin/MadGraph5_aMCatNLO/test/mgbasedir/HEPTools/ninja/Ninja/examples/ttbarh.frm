CF globabbr;
auto S Qsp, spa, spb;
auto V spv;
V Q, k1,...,k5,l3,l4,l5,e1,e2;
S Mu2;

L TTbarHDiagram  = globabbr(57) + Qspe2*globabbr(15) + Qspe1*globabbr(30) + 
      Qspe1*Qspe2*globabbr(48) + Qspl5*globabbr(120) + Qspl5*Qspe2*
      globabbr(79) + Qspl5*Qspe1*globabbr(129) + Qspl5*Qspe1*Qspe2*
      globabbr(131) + Qspl4*globabbr(91) + Qspl4*Qspe2*globabbr(100) + Qspl4*
      Qspe1*globabbr(26) + Qspl4*Qspe1*Qspe2*globabbr(146) - Qspl4*Qspl5*
      globabbr(132) + Qspk2*globabbr(22) + Qspk2*Qspe2*globabbr(39) + Qspk2*
      Qspe1*globabbr(50) + Qspk2*Qspe1*Qspe2*globabbr(52) + Qspk2*Qspl5*
      globabbr(96) + Qspk2*Qspl4*globabbr(137) + Qspk2^2*globabbr(6) + Qspk1*
      globabbr(21) + Qspk1*Qspe2*globabbr(40) + Qspk1*Qspe1*globabbr(115) + 
      Qspk1*Qspe1*Qspe2*globabbr(123) + Qspk1*Qspl5*globabbr(133) - Qspk1*
      Qspl4*globabbr(140) + Qspk1*Qspk2*globabbr(72) - Qspk1^2*globabbr(133)
       + QspQ*globabbr(13) + QspQ*Qspe2*globabbr(25) + QspQ*Qspe1*
      globabbr(36) + QspQ*Qspe1*Qspe2*globabbr(53) + QspQ*Qspl5*globabbr(8)
       + QspQ*Qspl4*globabbr(62) + QspQ*Qspk2*globabbr(11) + QspQ*Qspk1*
      globabbr(59) + QspQ^2*globabbr(75) + Qspval5e2*Qspe1*globabbr(46) + 
      Qspval5e2*Qspl5*Qspe1*globabbr(97) + Qspval5e2*Qspl4*Qspe1*
      globabbr(104) + Qspval5e2*Qspk2*Qspe1*globabbr(45) - Qspval5e2*Qspk1*
      Qspe1*globabbr(97) - Qspval5e2*QspQ*Qspe1*globabbr(102) + Qspval5e1*
      globabbr(98) + Qspval5e1*Qspe2*globabbr(73) + Qspval5e1*Qspl5*Qspe2*
      globabbr(126) + Qspval5e1*Qspl4*Qspe2*globabbr(87) - Qspval5e1*Qspk2*
      globabbr(81) + Qspval5e1*Qspk2*Qspe2*globabbr(84) + Qspval5e1*Qspk1*
      globabbr(81) - Qspval5e1*Qspk1*Qspe2*globabbr(126) + Qspval5e1*QspQ*
      globabbr(10) + Qspval5e1*QspQ*Qspe2*globabbr(12) + Qspvae2k2*Qspe1*
      globabbr(33) + Qspvae2k2*Qspl5*Qspe1*globabbr(19) + Qspvae2k2*Qspl4*
      Qspe1*globabbr(143) + Qspvae2k2*Qspk2*Qspe1*globabbr(86) - Qspvae2k2*
      Qspk1*Qspe1*globabbr(19) - Qspvae2k2*QspQ*Qspe1*globabbr(32) + 
      Qspvae1k2*globabbr(92) + Qspvae1k2*Qspe2*globabbr(43) + Qspvae1k2*
      Qspl5*Qspe2*globabbr(54) + Qspvae1k2*Qspl4*Qspe2*globabbr(58) + 
      Qspvae1k2*Qspk2*globabbr(88) + Qspvae1k2*Qspk2*Qspe2*globabbr(63) - 
      Qspvae1k2*Qspk1*globabbr(88) - Qspvae1k2*Qspk1*Qspe2*globabbr(54) + 
      Qspvae1k2*QspQ*globabbr(20) + Qspvae1k2*QspQ*Qspe2*globabbr(24) + 
      Qspval5l4*globabbr(44) + Qspval5l4*Qspe2*globabbr(67) + Qspval5l4*
      Qspe1*globabbr(68) + Qspval5l4*Qspe1*Qspe2*globabbr(74) - Qspval5l4*
      Qspl5*globabbr(77) + Qspval5l4*Qspl4*globabbr(7) + Qspval5l4*Qspk2*
      globabbr(47) + Qspval5l4*Qspk1*globabbr(77) + Qspval5l4*QspQ*
      globabbr(49) + Qspval5k2*globabbr(64) + Qspval5k2*Qspe2*globabbr(60) + 
      Qspval5k2*Qspe1*globabbr(9) + Qspval5k2*Qspe1*Qspe2*globabbr(38) + 
      Qspval5k2*Qspl4*globabbr(34) - Qspval5k2*Qspk2*globabbr(153) + 
      Qspval5k2*QspQ*globabbr(51) + Qspval5k1*globabbr(16) + Qspval5k1*
      Qspe2*globabbr(28) + Qspval5k1*Qspe1*globabbr(23) + Qspval5k1*Qspe1*
      Qspe2*globabbr(27) + Qspval5k1*Qspl4*globabbr(35) + Qspval5k1*Qspk2*
      globabbr(37) + Qspval5k1*QspQ*globabbr(55) + Qspval4k2*Qspe2*
      globabbr(65) + Qspval4k2*Qspe1*globabbr(18) + Qspvak2l4*Qspe1*Qspe2*
      globabbr(71) + Qspvak2k1*Qspe1*Qspe2*globabbr(66) + Qspvak1l4*Qspe1*
      Qspe2*globabbr(76) + Qspvak1k2*globabbr(17) + Qspvak1k2*Qspe2*
      globabbr(14) + Qspvak1k2*Qspe1*globabbr(29) + Qspvak1k2*Qspe1*Qspe2*
      globabbr(31) + Qspvak1k2*Qspl4*globabbr(42) + Qspvak1k2*Qspk2*
      globabbr(56) + Qspvak1k2*QspQ*globabbr(41);

Id QspQ = Q.Q;
Id Qspk1 = Q.k1;
Id Qspk2 = Q.k2;
Id Qspk3 = Q.k3;
Id Qspl3 = Q.l3;
Id Qspk4 = Q.k4;
Id Qspl4 = Q.l4;
Id Qspk5 = Q.k5;
Id Qspl5 = Q.l5;
Id Qspe1 = Q.e1;
Id Qspe2 = Q.e2;
Id Qspvak1k2 = Q.spvak1k2;
Id Qspvak1l3 = Q.spvak1l3;
Id Qspvak1l4 = Q.spvak1l4;
Id Qspvak1l5 = Q.spvak1l5;
Id Qspvak2k1 = Q.spvak2k1;
Id Qspvak2l3 = Q.spvak2l3;
Id Qspvak2l4 = Q.spvak2l4;
Id Qspvak2l5 = Q.spvak2l5;
Id Qspval3k1 = Q.spval3k1;
Id Qspval3k2 = Q.spval3k2;
Id Qspval3l4 = Q.spval3l4;
Id Qspval3l5 = Q.spval3l5;
Id Qspval4k1 = Q.spval4k1;
Id Qspval4k2 = Q.spval4k2;
Id Qspval4l3 = Q.spval4l3;
Id Qspval4l5 = Q.spval4l5;
Id Qspval5k1 = Q.spval5k1;
Id Qspval5k2 = Q.spval5k2;
Id Qspval5l3 = Q.spval5l3;
Id Qspval5l4 = Q.spval5l4;
Id Qspvak1e1 = Q.spvak1e1;
Id Qspvae1k1 = Q.spvae1k1;
Id Qspvak1e2 = Q.spvak1e2;
Id Qspvae2k1 = Q.spvae2k1;
Id Qspvak2e1 = Q.spvak2e1;
Id Qspvae1k2 = Q.spvae1k2;
Id Qspvak2e2 = Q.spvak2e2;
Id Qspvae2k2 = Q.spvae2k2;
Id Qspval3e1 = Q.spval3e1;
Id Qspvae1l3 = Q.spvae1l3;
Id Qspval3e2 = Q.spval3e2;
Id Qspvae2l3 = Q.spvae2l3;
Id Qspval4e1 = Q.spval4e1;
Id Qspvae1l4 = Q.spvae1l4;
Id Qspval4e2 = Q.spval4e2;
Id Qspvae2l4 = Q.spvae2l4;
Id Qspval5e1 = Q.spval5e1;
Id Qspvae1l5 = Q.spvae1l5;
Id Qspval5e2 = Q.spval5e2;
Id Qspvae2l5 = Q.spvae2l5;
Id Qspvae1e2 = Q.spvae1e2;
Id Qspvae2e1 = Q.spvae2e1;


// simple_test.cc

#include <iostream>
#include <ninja/ninja.hh>
#include <ninja/tensor_ninja.hh>
#include <ninja/rambo.hh>
using namespace ninja;

// fills components of t with numbers, defined below main
void fill_tensor(Complex  * t);

int main()
{
  // External legs of the loop
  const int N_LEGS = 4;

  // Center of mass energy
  const Real ENERGY_CM = 2;

  // Invariant s
  const Real S = ENERGY_CM*ENERGY_CM;

  // Rank of the numerator
  const int RANK = 5;

  // Declare an instance of the numerator
  Complex tenc[126];
  fill_tensor(tenc);
  TensorNumerator t(N_LEGS,RANK,tenc);

  // Define external momenta
  RealMomentum k[N_LEGS];

  // Create an instance of Rambo phase-space generator
  Rambo phase_space(S,N_LEGS);

  // get a random phase-space point
  phase_space.getMomenta(k);

  // define the internal momenta of the loop
  ninja::RealMomentum pi[N_LEGS];
  pi[0] = ninja::RealMomentum(0,0,0,0);
  pi[1] = k[0];
  pi[2] = k[0]+k[1];
  pi[3] = k[3];

  // define the internal masses
  ninja::Real msq[N_LEGS] = {1.,2.,3.,4.};

  // create an amplitude object
  ninja::Amplitude<RealMasses> amp(N_LEGS,RANK,pi,msq);

#if 1 // define S-Matrix from array...

  Real s_mat[N_LEGS*N_LEGS] = {0, 0, 2*mp(k[0],k[1]), 0,
                               0, 0, 0, -2*mp(k[0],k[3]),
                               2*mp(k[0],k[1]), 0, 0, 0,
                               0, -2*mp(k[0],k[3]), 0, 0};
  amp.setSMatrix(s_mat);

#else // ...or from an SMatrix object

  SMatrix s_mat ;
  s_mat.allocate(N_LEGS); // allocate the matrix
  s_mat.fill (0); // fill the entries with zeros
  s_mat(0,2) = s_mat(2 ,0) = 2*mp(k[0],k[1]);
  s_mat(1,3) = s_mat(3 ,1) = -2*mp(k[0],k[3]);
  amp.setSMatrix(s_mat);

#endif

  setVerbosity(Verbose::ALL);
  setTest(Test::ALL);

  // evaluate the integral
  amp.evaluate(t);

  // print the result
  std::cout << "Finite:      " << amp[0] // or amp.eps0(),  finite part
            << std::endl
            << "Single pole: " << amp[1] // or amp.epsm1(), single-pole
            << std::endl
            << "Double pole: " << amp[2] // or amp.epsm2(), double-pole
            << std::endl;

  return 0;
}


void fill_tensor(Complex  * t)
{
  t[0] = Complex(0.540172710654,1.20939586274);
  t[1] = Complex(1.15296156804,0.730431550533);
  t[2] = Complex(0.892529292411,1.44222417899);
  t[3] = Complex(0.944411079449,1.02843406311);
  t[4] = Complex(0.825836112081,0.604129625489);
  t[5] = Complex(1.34958474301,1.36280073434);
  t[6] = Complex(1.11663625142,0.966643871469);
  t[7] = Complex(0.935943755498,1.1879326709);
  t[8] = Complex(0.770518596901,1.11971365917);
  t[9] = Complex(0.682828591012,0.995375656853);
  t[10] = Complex(0.776390458256,1.42147441174);
  t[11] = Complex(0.763704574735,0.670707859924);
  t[12] = Complex(0.824684847089,0.536022090317);
  t[13] = Complex(1.38638001922,1.05868420362);
  t[14] = Complex(0.506964461257,0.908153437529);
  t[15] = Complex(0.587009349702,0.621468715381);
  t[16] = Complex(0.982990452656,0.707297250182);
  t[17] = Complex(1.06894844327,0.826203828869);
  t[18] = Complex(1.12210448817,0.635696047696);
  t[19] = Complex(1.17295599108,1.12783129702);
  t[20] = Complex(1.39333745503,0.79431968239);
  t[21] = Complex(1.35304587296,1.15657457921);
  t[22] = Complex(0.893568323537,0.905152924419);
  t[23] = Complex(1.38770661159,0.758696295657);
  t[24] = Complex(0.531016116347,1.07124573774);
  t[25] = Complex(1.45713892328,0.716495288706);
  t[26] = Complex(0.758263244749,1.30869577383);
  t[27] = Complex(1.3967183981,0.931948077397);
  t[28] = Complex(1.37503043723,1.4158156236);
  t[29] = Complex(1.18880212899,1.16142422563);
  t[30] = Complex(1.36948120203,0.504329937111);
  t[31] = Complex(1.1394663635,0.644859787054);
  t[32] = Complex(1.32809968824,1.1449360416);
  t[33] = Complex(0.693867994005,0.64581216723);
  t[34] = Complex(0.82747004087,0.638889300854);
  t[35] = Complex(1.05782412233,1.35297152229);
  t[36] = Complex(0.949876289588,1.33035187843);
  t[37] = Complex(1.04431196847,0.614482796914);
  t[38] = Complex(0.577481247295,1.38725541372);
  t[39] = Complex(0.629776982426,0.92875235081);
  t[40] = Complex(1.26495914534,1.49976302562);
  t[41] = Complex(1.33760976502,1.3217264418);
  t[42] = Complex(0.819197947546,0.682803796055);
  t[43] = Complex(1.27027382256,1.4947685382);
  t[44] = Complex(1.22807296587,0.608557029258);
  t[45] = Complex(1.37157105835,0.761674602182);
  t[46] = Complex(1.35112898748,0.72676496136);
  t[47] = Complex(1.05153270244,1.30349603799);
  t[48] = Complex(1.49384370547,0.526767747491);
  t[49] = Complex(0.857812516921,1.08492527462);
  t[50] = Complex(0.962983207272,0.918829080264);
  t[51] = Complex(0.914929023187,0.991194608347);
  t[52] = Complex(0.838556042275,0.513762365848);
  t[53] = Complex(0.664807660294,1.07236056015);
  t[54] = Complex(0.943956241739,0.527286895573);
  t[55] = Complex(1.32476742748,1.13609130398);
  t[56] = Complex(1.39880755508,1.1352233102);
  t[57] = Complex(0.824528810757,1.05020837283);
  t[58] = Complex(0.709718462275,0.567761101408);
  t[59] = Complex(0.599045434733,0.970331239136);
  t[60] = Complex(1.2238970917,0.972040066341);
  t[61] = Complex(0.616304272247,0.637001208268);
  t[62] = Complex(0.656885360288,1.21303733466);
  t[63] = Complex(0.651516596883,0.533831120028);
  t[64] = Complex(1.23163088673,1.28917225419);
  t[65] = Complex(0.616222813136,0.805848724256);
  t[66] = Complex(1.13459124787,0.57793697367);
  t[67] = Complex(0.841381544897,1.11405834305);
  t[68] = Complex(1.03227708054,0.744011353527);
  t[69] = Complex(0.605154323438,0.724940135862);
  t[70] = Complex(0.854134101961,0.924427199907);
  t[71] = Complex(1.26658388847,0.935615244211);
  t[72] = Complex(0.700462892245,1.44138001318);
  t[73] = Complex(1.0476723212,1.39377821849);
  t[74] = Complex(0.973731599485,0.948434589214);
  t[75] = Complex(1.14805362771,1.26097136903);
  t[76] = Complex(1.03105493559,1.37014531092);
  t[77] = Complex(1.32341348328,1.44221248383);
  t[78] = Complex(0.988566873594,0.566969737947);
  t[79] = Complex(0.505031985916,0.586229307457);
  t[80] = Complex(1.01933525929,0.893935417906);
  t[81] = Complex(1.19411199177,0.811410360117);
  t[82] = Complex(0.743752162871,0.82894680764);
  t[83] = Complex(1.38877890184,1.4157577325);
  t[84] = Complex(1.01654097952,1.05206093313);
  t[85] = Complex(0.905306041337,0.775002125606);
  t[86] = Complex(0.517249745117,1.37275982396);
  t[87] = Complex(1.2309844479,1.07140642194);
  t[88] = Complex(1.41108024135,0.718491466996);
  t[89] = Complex(0.61846540041,0.607928819939);
  t[90] = Complex(1.16376490579,0.913152174442);
  t[91] = Complex(1.32544841853,0.933587124734);
  t[92] = Complex(0.937251922494,0.728915969142);
  t[93] = Complex(1.03485495101,0.703519995639);
  t[94] = Complex(1.34076981638,1.15382439238);
  t[95] = Complex(1.47303030722,0.882908091469);
  t[96] = Complex(1.39364494658,1.22128847728);
  t[97] = Complex(1.14897580144,1.04995808049);
  t[98] = Complex(1.0172252889,0.696164282135);
  t[99] = Complex(1.40412084563,1.20979700385);
  t[100] = Complex(0.91660191608,0.901792052273);
  t[101] = Complex(0.552393538696,0.556293590662);
  t[102] = Complex(0.663430601167,1.18225913569);
  t[103] = Complex(1.12482119671,0.788099561641);
  t[104] = Complex(1.406818623,1.3305826111);
  t[105] = Complex(1.11468654218,0.672328360085);
  t[106] = Complex(1.4843754317,1.30037187748);
  t[107] = Complex(0.586620121388,0.607806675693);
  t[108] = Complex(0.694360245008,1.32529191703);
  t[109] = Complex(0.61423579166,0.752153516757);
  t[110] = Complex(1.44341493782,1.26763746981);
  t[111] = Complex(1.40927965616,0.682996806338);
  t[112] = Complex(0.870539424147,0.872971647926);
  t[113] = Complex(1.22139962495,1.40625198162);
  t[114] = Complex(1.24735644542,0.630031934683);
  t[115] = Complex(0.760004240722,0.524495272272);
  t[116] = Complex(0.642982041626,0.510576118645);
  t[117] = Complex(0.540277710807,1.46569686511);
  t[118] = Complex(1.45426621579,1.37589376261);
  t[119] = Complex(1.49438134769,1.39154924852);
  t[120] = Complex(0.960705883556,1.05902648993);
  t[121] = Complex(1.21845675516,1.39958865831);
  t[122] = Complex(0.734223362079,1.48761818932);
  t[123] = Complex(0.933461901289,1.17855916862);
  t[124] = Complex(1.23884680172,0.636560155203);
  t[125] = Complex(1.43833393524,1.13466988995);
}

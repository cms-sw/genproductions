// This file implements most of the private methods of the Amplitude
// class declared in ninja.hh.


// note: If the library is configured enabling the higher-rank
// support, this file is included in both ninja.cc and Xninja.cc.  The
// preprocessor macro NINJA_IMPLEMENTING_X1RANK is defined only in the
// latter and it is used to distinguish between the normal-rank and
// higher-rank implementations.


/////////////////
// Denominator //
/////////////////

namespace {

  // D = (q+V)^2 - m^2 - \mu^2

  template<typename MassType>
  inline Complex Den(const ComplexMomentum & q, const RealMomentum & V,
                     const MassType & m2)
  {
    return mp2(q+V)-m2;
  }

  template<typename MassType>
  inline Complex Den(const ComplexMomentum & q, const RealMomentum & V,
                     const MassType & m2, const Real & muq)
  {
    return mp2(q+V)-m2-muq;
  }

  template<typename MassType>
  inline Complex Den(const ComplexMomentum & q, const RealMomentum & V,
                     const MassType & m2, const Complex & muq)
  {
    return mp2(q+V)-m2-muq;
  }

  // Writes the coefficients c[i] of the expansion of an uncut
  // denominator D = d.d0/t + d.d1 + d.d2*t, with q = a + e3*t + e4eff/t.
  // Here 'pden' and 'mden' are the momentum and masses which identify
  // the denominator D, while 'pcut' and 'mcut' are the momentum and
  // masses which identify a cut denominator.
  template<typename MassType>
  inline void exDenL (const ComplexMomentum & a,
                      const ComplexMomentum & e3,
                      const ComplexMomentum & e4eff,
                      const Complex & param,
                      const RealMomentum & pden, const MassType & mdenq,
                      const RealMomentum & pcut, const MassType & mcutq,
                      DenExp<3> & d)
  {
    RealMomentum pdiff = pden-pcut;
    d.d0 = TWO*mp(e3,pdiff); // O(t) term
    d.d1 = TWO*mp(a,pdiff) + mp(pden,pden) - mp(pcut,pcut)
      - mdenq + mcutq; // const term
    d.d2[1] = TWO*mp(e4eff,pdiff); // O(mu^2/t) term
    d.d2[0] = param*d.d2[1]; // O(1/t) term
  }

  // Same as the previous one, this time for bubbles
  template<typename MassType>
  inline void exDenL (const ComplexMomentum & e1eff,
                      const ComplexMomentum & e2eff,
                      const ComplexMomentum & e3,
                      const ComplexMomentum & e4eff,
                      const Complex param[3],
                      const RealMomentum & pden, const MassType & mdenq,
                      const RealMomentum & pcut, const MassType & mcutq,
                      DenExp<2> & d)
  {
    RealMomentum pdiff = pden-pcut;
    Real p2diff = mp(pden,pden) - mp(pcut,pcut);
    d.d0 = TWO*mp(e3,pdiff); // O(t) term
    d.d1[0] = TWO*mp(e1eff,pdiff) + p2diff + (- mdenq + mcutq); // const term
    d.d1[1] = TWO*mp(e2eff,pdiff); // O(x) term
    d.d2[1] = TWO*mp(e4eff,pdiff); // O(mu^2/t) term
    d.d2[0] = param[0]*d.d2[1]; // O(1/t) term
    d.d2[2] = param[1]*d.d2[1]; // O(x/t) term
    d.d2[3] = param[2]*d.d2[1]; // O(x^2/t) term
  }

} // namespace




///////////////////
///////////////////
//// Reduction ////
///////////////////
///////////////////

#define MU_MASS_TYPE typename details::common_type<MassType,Real>::type

namespace {

// maximum_rank_allowed = n+max_hr
const int max_hr = 0
#ifdef NINJA_IMPLEMENTING_X1RANK
    +1
#endif
    ;

} // namespace



///////////////
// pentagons //
///////////////

template<typename MassType>
void Amplitude<MassType>::evaluatePentagons (Numerator & num,
                                             CutsVector<Pentagon> & pen)
{

  // loop over Partitions
  CutsVector<Pentagon>::iterator this_cut = pen.begin();
  for (PartitionInt cut1=0; cut1<n-5+1;++cut1)
    for (PartitionInt cut2=cut1+1; cut2<n; ++cut2)
      for (PartitionInt cut3=cut2+1; cut3<n; ++cut3)
        for (PartitionInt cut4=cut3+1; cut4<n; ++cut4)
          for (PartitionInt cut5=cut4+1; cut5<n; ++cut5) {

            // Partition p and complementary partition cp
            (*this_cut).p[0] = cut1;
            (*this_cut).p[1] = cut2;
            (*this_cut).p[2] = cut3;
            (*this_cut).p[3] = cut4;
            (*this_cut).p[4] = cut5;
            complementaryPartition((*this_cut).p, 5, n, (*this_cut).cp);

            // evaluate
            evaluatePentagon(num,*this_cut);

            // move to next cut
            ++this_cut;

          }
}

template<typename MassType>
void Amplitude<MassType>::evaluatePentagon (Numerator & num,
                                            Pentagon & pen)
{

  PartitionInt cut1 = pen.p[0];
  PartitionInt cut2 = pen.p[1];
  PartitionInt cut3 = pen.p[2];
  PartitionInt cut4 = pen.p[3];
  PartitionInt cut5 = pen.p[4];

  // computing external momenta
  RealMomentum p[5] = {V[cut2]-V[cut1],
                       V[cut3]-V[cut2],
                       V[cut4]-V[cut3],
                       V[cut5]-V[cut4],
                       V[cut1]-V[cut5]};

  // Get basis and other relevant momenta
  if (!stability_check(gram_det(p[4],p[0]))) return;
  Basis e(p[4],p[0]);

  // computing the solution l for the loop momentum
  ComplexMomentum l;
  Complex muq;
  CutPentagon<MassType>(p[4], p[0], p[1], p[3], e,
                        m2[cut1], m2[cut2], m2[cut3],
                        m2[cut4], m2[cut5]
                        ).getLoopMomentum(l, muq);

  // get the solution for q, mu^2 and evaluate the numerator
  ComplexMomentum q = l - V[cut1];
  Complex N = num.evaluate(q, muq, 5, pen.p);

  // divide by the remaining denominators
  Complex D = ONE;
  for (int i =0;i<n-5;++i)
    D *= Den(q, V[pen.cp[i]], m2[pen.cp[i]], muq);

  // and store the results
  if (!stability_check(D)) return;
  pen.c[0] = N/D/muq;

}



///////////
// boxes //
///////////

template<typename MassType>
void Amplitude<MassType>::evaluateBoxes(Numerator & num,
                                        const CutsVector<Pentagon> & pent,
                                        CutsVector<Box> & d)
{

  // loop over Partitions
  CutsVector<Box>::iterator this_cut = d.begin();
  for (PartitionInt cut1=0; cut1<n-4+1;++cut1)
    for (PartitionInt cut2=cut1+1; cut2<n; ++cut2)
      for (PartitionInt cut3=cut2+1; cut3<n; ++cut3)
        for (PartitionInt cut4=cut3+1; cut4<n; ++cut4) {

          // Partition p and complementary partition cp
          (*this_cut).p[0] = cut1;
          (*this_cut).p[1] = cut2;
          (*this_cut).p[2] = cut3;
          (*this_cut).p[3] = cut4;
          complementaryPartition((*this_cut).p, 4, n, (*this_cut).cp);

          // evaluate
          evaluateBox(num, pent, *this_cut);

          // move to next cut
          ++this_cut;

        }
}

template<typename MassType>
void Amplitude<MassType>::evaluateBox(Numerator & num,
                                      const CutsVector<Pentagon> & pent,
                                      Box & d)
{
  PartitionInt cut1 = d.p[0];
  PartitionInt cut2 = d.p[1];
  PartitionInt cut3 = d.p[2];
  PartitionInt cut4 = d.p[3];

  // computing external momenta
  RealMomentum p[4] = {V[cut2]-V[cut1],
                       V[cut3]-V[cut2],
                       V[cut4]-V[cut3],
                       V[cut1]-V[cut4]};

  // Invariants
  Real invariants[] = {ninja::real(m2[cut1]),
                       ninja::real(m2[cut2]),
                       ninja::real(m2[cut3]),
                       ninja::real(m2[cut4]),
                       abs(s_mat(cut2,cut1)),
                       abs(s_mat(cut3,cut2)),
                       abs(s_mat(cut4,cut3)),
                       abs(s_mat(cut1,cut4)),
                       abs(s_mat(cut3,cut1)),
                       abs(s_mat(cut4,cut2))};

  Real scale_choice = *max_element(invariants,invariants + 10);
  if (abs(scale_choice) < INFRARED_EPS)
    scale_choice = ONE;

  // Get basis and other relevant momenta
  if (!stability_check(gram_det(p[3],p[0]))) return;
  Basis e(p[3],p[0]);
  d.Vort = mp(p[1],e.e4)*e.e3 - mp(p[1],e.e3)*e.e4;
  if (!stability_check(mp2(d.Vort))) return;
  d.V0 = V[cut1];

  // get the two solutions for the loop momentum
  ComplexMomentum l[2];
  CutBox<MassType> (p[3], p[0], p[1], e,
                    m2[cut1], m2[cut2], m2[cut3], m2[cut4]
                    ).getLoopMomentum(l[0], l[1]);

  // evaluate integrand in the first solution for q
  ComplexMomentum q = l[0] - V[cut1];
  Complex N1 = num.evaluate(q, ZERO, 4, d.p);
  Complex D = ONE;
  for (int i =0; i<n-4; ++i)
    D *= Den(q, V[d.cp[i]], m2[d.cp[i]]);
  if (!stability_check(D)) return;
  N1 = N1/D;

  // evaluate integrand in the second solution for q
  q = l[1] - V[cut1];
  Complex N2 = num.evaluate(q, ZERO, 4, d.p);
  D = ONE;
  for (int i =0; i<n-4; ++i)
    D *= Den(q, V[d.cp[i]], m2[d.cp[i]]);
  if (!stability_check(D)) return;
  N2 = N2/D;

  // store the results
  d.c[0] = HALF*(N1+N2);


  // rational term with mu^4
  if (rank >= n && use_mu_exp) { // can only appear if rank >= n

#ifndef NINJA_IMPLEMENTING_X1RANK

    // normal rank muExpansion

    // compute numerator expansion in mu^2
    Complex Vort2 = mp(d.Vort,d.Vort);
    Complex nmu;
    num.muExpansion(&d.Vort,d.p,&nmu);

    // compute denominator expansion in mu^2
    Complex den = ONE;
    for (int i =0; i<n-4; ++i) {
      int uncut = d.cp[i];
      RealMomentum kk = V[uncut]-d.V0;
      den *= TWO*mp(d.Vort,kk);
    }

    // store the result
    if (!stability_check(den)) return;
    d.c[4] = nmu/den/(Vort2*Vort2);

#else // !NINJA_IMPLEMENTING_X1RANK

    // higher rank muExpansion

    // compute numerator expansion in mu^2
    Complex Vort2 = mp(d.Vort,d.Vort);
    ComplexMomentum qmuexp[2] = {d.Vort,-V[cut1] + HALF*(l[0]+l[1])};
    Complex nmu[2];
    num.muExpansion(qmuexp,d.p,nmu);

    // polynomial division in mu^2
    Complex den[2];
    for (int i =0; i<n-4; ++i) {
      int uncut = d.cp[i];
      RealMomentum kk = V[uncut]-d.V0;
      den[0] = TWO*mp(d.Vort,kk);
      if (!stability_check(den[0])) return;
      den[1] = mp2(V[uncut]) - mp2(V[cut1]) - m2[uncut] + m2[cut1]
        + TWO*mp(qmuexp[1],kk);
      nmu[0] /= den[0];
      nmu[1] -= den[1]*nmu[0];
      nmu[1] /= den[0];
    }

    // store the result
    d.c[4] = nmu[1]/(Vort2*Vort2);

#endif // NINJA_IMPLEMENTING_X1RANK

  } else if (rank >= n) {

      // Rational part via numerator sampling

      // mu^2 = 1
      Real muq = scale_choice;

      // get the two solutions for the loop momentum
      CutBox<MU_MASS_TYPE>(p[3], p[0], p[1], e,
                           m2[cut1]+muq, m2[cut2]+muq,
                           m2[cut3]+muq, m2[cut4]+muq
                          ).getLoopMomentum(l[0], l[1]);

      // evaluate integrand in the first solution for q
      q = l[0] - V[cut1];
      N1 = num.evaluate(q, muq, 4, d.p);
      D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
      // and subtract pentagons
      for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
        if (isSubPartition4of5(d.p,(*i).p)) {
          Complex sub = (*i).poly(q,muq);
          for (int j = 0; j<n-5; ++j)
            sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
          N1-=sub;
        }
      if (!stability_check(D)) return;
      N1 = N1/D;

      // evaluate integrand in the second solution for q
      q = l[1] - V[cut1];
      N2 = num.evaluate(q, muq, 4, d.p);
      D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
      // and subtract pentagons
      for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
        if (isSubPartition4of5(d.p,(*i).p)) {
          Complex sub = (*i).poly(q,muq);
          for (int j = 0; j<n-5; ++j)
            sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
          N2-=sub;
        }
      if (!stability_check(D)) return;
      N2 = N2/D;

      // store the results
      // d2 + d4*scale_choice
      d.c[2] = (HALF*(N1+N2) - d.c[0])/scale_choice;

      // mu^2 = -1
      muq = -scale_choice;

      // get the two solutions for the loop momentum
      CutBox<MU_MASS_TYPE>(p[3], p[0], p[1], e,
                           m2[cut1]+muq, m2[cut2]+muq,
                           m2[cut3]+muq, m2[cut4]+muq
                          ).getLoopMomentum(l[0], l[1]);

      // evaluate integrand in the first solution for q
      q = l[0] - V[cut1];
      N1 = num.evaluate(q, muq, 4, d.p);
      D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
      // and subtract pentagons
      for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
        if (isSubPartition4of5(d.p,(*i).p)) {
          Complex sub = (*i).poly(q,muq);
          for (int j = 0; j<n-5; ++j)
            sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
          N1-=sub;
        }
      if (!stability_check(D)) return;
      N1 = N1/D;

      // evaluate integrand in the second solution for q
      q = l[1] - V[cut1];
      N2 = num.evaluate(q, muq, 4, d.p);
      D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
      // and subtract pentagons
      for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
        if (isSubPartition4of5(d.p,(*i).p)) {
          Complex sub = (*i).poly(q,muq);
          for (int j = 0; j<n-5; ++j)
            sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
          N2-=sub;
        }
      if (!stability_check(D)) return;
      N2 = N2/D;

      // store the results
      // -d2 + d4*scale_choice
      d.c[4] = (HALF*(N1+N2) - d.c[0])/scale_choice;
      d.c[4] = HALF*(d.c[4] + d.c[2])/scale_choice;

    }

}


template<typename MassType>
void Amplitude<MassType>::evaluateFullBoxes(Numerator & num,
                                            const CutsVector<Pentagon> & pent,
                                            CutsVector<Box> & d)
{

  // loop over Partitions
  CutsVector<Box>::iterator this_cut = d.begin();
  for (PartitionInt cut1=0; cut1<n-4+1;++cut1)
    for (PartitionInt cut2=cut1+1; cut2<n; ++cut2)
      for (PartitionInt cut3=cut2+1; cut3<n; ++cut3)
        for (PartitionInt cut4=cut3+1; cut4<n; ++cut4) {

          // Partition p and complementary partition cp
          (*this_cut).p[0] = cut1;
          (*this_cut).p[1] = cut2;
          (*this_cut).p[2] = cut3;
          (*this_cut).p[3] = cut4;
          complementaryPartition((*this_cut).p, 4, n, (*this_cut).cp);

          // evaluate
          evaluateFullBox(num, pent, *this_cut);

          // move to next cut
          ++this_cut;

        }
}


template<typename MassType>
void Amplitude<MassType>::evaluateFullBox(Numerator & num,
                                          const CutsVector<Pentagon> & pent,
                                          Box & d)
{
  PartitionInt cut1 = d.p[0];
  PartitionInt cut2 = d.p[1];
  PartitionInt cut3 = d.p[2];
  PartitionInt cut4 = d.p[3];

  // computing external momenta
  RealMomentum p[4] = {V[cut2]-V[cut1],
                       V[cut3]-V[cut2],
                       V[cut4]-V[cut3],
                       V[cut1]-V[cut4]};

  // Invariants
  Real invariants[] = {ninja::real(m2[cut1]),
                       ninja::real(m2[cut2]),
                       ninja::real(m2[cut3]),
                       ninja::real(m2[cut4]),
                       abs(s_mat(cut2,cut1)),
                       abs(s_mat(cut3,cut2)),
                       abs(s_mat(cut4,cut3)),
                       abs(s_mat(cut1,cut4)),
                       abs(s_mat(cut3,cut1)),
                       abs(s_mat(cut4,cut2))};

  Real scale_choice = *max_element(invariants,invariants + 10);
  if (abs(scale_choice) < INFRARED_EPS)
    scale_choice = ONE;

  // Get basis and other relevant momenta
  if (!stability_check(gram_det(p[3],p[0]))) return;
  Basis e(p[3],p[0]);
  d.Vort = mp(p[1],e.e4)*e.e3 - mp(p[1],e.e3)*e.e4;
  if (!stability_check(mp2(d.Vort))) return;
  d.V0 = V[cut1];

  // get the two solutions for the loop momentum
  ComplexMomentum l[2];
  CutBox<MassType>(p[3], p[0], p[1], e,
                   m2[cut1], m2[cut2], m2[cut3], m2[cut4]
                  ).getLoopMomentum(l[0], l[1]);

  // evaluate integrand in the first solution for q
  ComplexMomentum q = l[0] - V[cut1];
  Complex N1 = num.evaluate(q, ZERO, 4, d.p);
  Complex D = ONE;
  for (int i =0; i<n-4; ++i)
    D *= Den(q, V[d.cp[i]], m2[d.cp[i]]);
  if (!stability_check(D)) return;
  N1 = N1/D;

  // evaluate integrand in the second solution for q
  q = l[1] - V[cut1];
  Complex N2 = num.evaluate(q, ZERO, 4, d.p);
  D = ONE;
  for (int i =0;i<n-4;++i)
    D *= Den(q, V[d.cp[i]], m2[d.cp[i]]);
  if (!stability_check(D)) return;
  N2 = N2/D;

  // store the results
  d.c[0] = HALF*(N1+N2);
  Complex Vl1 = mp(l[0],d.Vort);
  Complex Vl2 = mp(l[1],d.Vort);
  d.c[1]= (N1-N2)/(Vl1-Vl2);

  // rational term with mu^4
  if (rank >= n && use_mu_exp) { // can only appear if rank >= n

#ifndef NINJA_IMPLEMENTING_X1RANK

    // compute numerator expansion in mu^2
    Complex Vort2 = mp(d.Vort,d.Vort);
    Complex nmu;
    num.muExpansion(&d.Vort,d.p,&nmu);

    // compute denonimator expansion in mu^2
    Complex den =ONE;
    for (int i =0;i<n-4;++i) {
      int uncut = d.cp[i];
      RealMomentum kk = V[uncut]-d.V0;
      den *= TWO*mp(d.Vort,kk);
    }

    // store the result
    if (!stability_check(den)) return;
    d.c[4] = nmu/den/(Vort2*Vort2);

#else

    // higher rank muExpansion

    // compute numerator expansion in mu^2
    Complex Vort2 = mp(d.Vort,d.Vort);
    //Complex Vort2 = Real(1.0);
    //ComplexMomentum qmuexp[2] = {d.Vort,-V[cut1] + 0.5*(l[0]+l[1])};
    ComplexMomentum qmuexp[2] = {d.Vort,-V[cut1] + HALF*(l[0]+l[1])};
    Complex nmu[2];
    num.muExpansion(qmuexp,d.p,nmu);

    // polynomial division in mu^2
    Complex den[2];
    for (int i =0;i<n-4;++i) {
      int uncut = d.cp[i];
      RealMomentum kk = V[uncut]-d.V0;
      den[0] = TWO*mp(d.Vort,kk);
      if (!stability_check(den[0])) return;
      den[1] = mp2(V[uncut]) - mp2(V[cut1]) - m2[uncut] + m2[cut1]
        + TWO*mp(qmuexp[1],kk);
      nmu[0] /= den[0];
      nmu[1] -= den[1]*nmu[0];
      nmu[1] /= den[0];
    }

    // store the result
    d.c[4] = nmu[1]/(Vort2*Vort2);
    d.c[5] = nmu[0]/(Vort2*Vort2*Vort2);

#endif

  }

  if (rank >= n-2) {

    // Rational part via numerator sampling

    // This is required in order to get all the spurious
    // coefficients of the box

    // mu^2 = 1
    Real muq = scale_choice;

    // get the two solutions for the loop momentum
    CutBox<MU_MASS_TYPE>(p[3], p[0], p[1], e,
                         m2[cut1]+muq, m2[cut2]+muq,
                         m2[cut3]+muq, m2[cut4]+muq
                        ).getLoopMomentum(l[0], l[1]);

    // evaluate integrand in the first solution for q
    q = l[0] - V[cut1];
    N1 = num.evaluate(q, muq, 4, d.p);
    D = ONE;
    for (int i =0;i<n-4;++i)
      D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
    // and subtract pentagons
    for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
      if (isSubPartition4of5(d.p,(*i).p)) {
        Complex sub = (*i).poly(q,muq);
        for (int j = 0; j<n-5; ++j)
          sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
        N1-=sub;
      }
    N1 = N1/D;

    // evaluate integrand in the second solution for q
    q = l[1] - V[cut1];
    N2 = num.evaluate(q, muq, 4, d.p);
    D = ONE;
    for (int i =0;i<n-4;++i)
      D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
    // and subtract pentagons
    for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
      if (isSubPartition4of5(d.p,(*i).p)) {
        Complex sub = (*i).poly(q,muq);
        for (int j = 0; j<n-5; ++j)
          sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
        N2-=sub;
      }
    N2 = N2/D;

    // store the results
    d.c[2] = (HALF*(N1+N2) - d.c[0])/scale_choice
      - d.c[4]*scale_choice;  // c2 (+ c4*scale_choice)
    Vl1 = mp(l[0],d.Vort);
    Vl2 = mp(l[1],d.Vort);
    d.c[3] = ((N1-N2)/(Vl1-Vl2) - d.c[1])
      /scale_choice;  // c3 (+ c5*scale_choice)
#ifdef NINJA_IMPLEMENTING_X1RANK
    d.c[3] -= d.c[5]*scale_choice;
#endif

  }

  if (!use_mu_exp && rank >= n) {

    // Another sampling is needed if the mu-expansion is not used

    // mu^2 = -1
    Real muq = -scale_choice;

    // get the two solutions for the loop momenta
    CutBox<MU_MASS_TYPE>(p[3], p[0], p[1], e,
                         m2[cut1]+muq, m2[cut2]+muq,
                         m2[cut3]+muq, m2[cut4]+muq
                        ).getLoopMomentum(l[0], l[1]);

    // evaluate integrand in the first solution for q
    q = l[0] - V[cut1];
    N1 = num.evaluate(q, muq, 4, d.p);
    D = ONE;
    for (int i =0;i<n-4;++i)
      D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
    // and subtract pentagons
    for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
      if (isSubPartition4of5(d.p,(*i).p)) {
        Complex sub = (*i).poly(q,muq);
        for (int j = 0; j<n-5; ++j)
          sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
        N1-=sub;
      }
    N1 = N1/D;

    // evaluate integrand in the second solution for q
    q = l[1] - V[cut1];
    N2 = num.evaluate(q, muq, 4, d.p);
    D = ONE;
    for (int i =0;i<n-4;++i)
      D *= Den(q, V[d.cp[i]], m2[d.cp[i]], muq);
    // and subtract pentagons
    for (PentagonsCIter i = pent.begin(); i!=pent.end(); ++i)
      if (isSubPartition4of5(d.p,(*i).p)) {
        Complex sub = (*i).poly(q,muq);
        for (int j = 0; j<n-5; ++j)
          sub *= Den(q, V[(*i).cp[j]], m2[(*i).cp[j]], muq);
        N2-=sub;
      }
    N2 = N2/D;

    // store the results
    d.c[4] = (HALF*(N1+N2) - d.c[0])
      /scale_choice;  // -c2 + c4*scale_choice
    d.c[4] = HALF*(d.c[2]
                    + d.c[4]); // = c4*scale_choice;
    d.c[2] = (d.c[2] - d.c[4]);
    d.c[4] /= scale_choice;
#ifdef NINJA_IMPLEMENTING_X1RANK
    Vl1 = mp(l[0],d.Vort);
    Vl2 = mp(l[1],d.Vort);
    d.c[5] = ((N1-N2)/(Vl1-Vl2) - d.c[1])
      /scale_choice;  // -c3 (+ c5*scale_choice)
    d.c[5] = HALF*(d.c[5] + d.c[3])
      /scale_choice;
    d.c[3] = d.c[3] - d.c[5];
#endif //  NINJA_IMPLEMENTING_X1RANK
  }

}



///////////////
// triangles //
///////////////

template<typename MassType>
void Amplitude<MassType>::evaluateTriangles(Numerator & num,
                                            CutsVector<Triangle> & c)
{

  // loop over Partitions
  CutsVector<Triangle>::iterator this_cut = c.begin();
  for (PartitionInt cut1=0; cut1<n-3+1;++cut1)
    for (PartitionInt cut2=cut1+1; cut2<n; ++cut2)
      for (PartitionInt cut3=cut2+1; cut3<n; ++cut3) {

        // Partition p and complementary partition cp
        (*this_cut).p[0] = cut1;
        (*this_cut).p[1] = cut2;
        (*this_cut).p[2] = cut3;
        complementaryPartition((*this_cut).p, 3, n, (*this_cut).cp);

        // evaluate
        evaluateTriangle(num, *this_cut);

        // move to next cut
        ++this_cut;

      }
}


template<typename MassType>
void Amplitude<MassType>::evaluateTriangle(Numerator & num,
                                           Triangle & c)
{

  const PartitionInt cut1 = c.p[0];
  const PartitionInt cut2 = c.p[1];
  const PartitionInt cut3 = c.p[2];

  // check whether linear, quadratic and cubic terms are present
  const bool lin = rank >= n-2;
  const bool quad = rank >= n-1;
  const bool cub = rank >= n;

  // computing external momenta
  RealMomentum p[3] = {V[cut2]-V[cut1],
                       V[cut3]-V[cut2],
                       V[cut1]-V[cut3]};

  // Get basis and other relevant momenta
  if (!stability_check(gram_det(p[2],p[0]))) return;
  Basis e(p[2],p[0]);
  c.e3 = e.e3;  c.e4 = e.e4;
  c.V0 = V[cut1];

  // get the parametric solutions for the loop momentum
  Complex param;
  ComplexMomentum amu;
  CutTriangle<MassType>(p[2], p[0], e,
                        m2[cut1], m2[cut2], m2[cut3]
                        ).getLoopMomentum(amu, param);
  amu = amu - c.V0; // a = a - p0

  // numexp[] stores the coefficients of the numerator expansion in
  // the order: {t^rank, t^(rank-1), t^(rank-2), t^(rank-2)*mu^2, ...}
  Complex numexp[6+3*max_hr];

  // numexp[jext`i'mu`j'] gets the t^`i'*mu^`j' term in the expansion
#ifndef NINJA_IMPLEMENTING_X1RANK
  // case: rank <= n
  const int jext3 = rank-n;
  const int jext2 = rank-n+1;
  const int jext1 = rank-n+2;
  const int jext1mu2 = 3; // when != 0
  const int jext0 = rank-n+3 + (cub ? 1 : 0);
  const int jext0mu2 = 3 + (cub ? 2 : 0);
#else
  // case: rank == n+1
  const int jext4 = 0;
  const int jext3 = 1;
  const int jext2 = 2;
  const int jext2mu2 = 3;
  const int jext1 = 4;
  const int jext1mu2 = 5;
  const int jext0 = 6;
  const int jext0mu2 = 7;
  const int jext0mu4 = 8;
#endif

  ComplexMomentum e4eff = HALF*c.e4/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,c.e3,e4eff,param,
                  rank-n+3,3,c.p,
                  numexp);

  // Uncut denoms
  DenExp<3> denc;

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-3; ++i) {
    const int nden = c.cp[i];
    exDenL(amu, e.e3, e4eff, param,
           V[nden], m2[nden], c.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+3+1, denc);
  }

  // store the coefficients
  c.c[0] = numexp[jext0];
  if (lin)
    c.c[4] = numexp[jext1]/e.mp34();
  if (quad) {
    c.c[5] = numexp[jext2]/e.mp34()/e.mp34();
    c.c[7] = numexp[jext0mu2];
  }
  if (cub) {
    c.c[6] = numexp[jext3]/e.mp34()/e.mp34()/e.mp34();
    c.c[9] = numexp[jext1mu2]/e.mp34();
  }
#ifdef NINJA_IMPLEMENTING_X1RANK
  c.c[13] = numexp[jext4]/e.mp34()/e.mp34()/e.mp34()/e.mp34();
  c.c[11] = numexp[jext2mu2]/e.mp34()/e.mp34();
  c.c[14] = numexp[jext0mu4];
#endif

  // swap e3 and e4 and repeat

  e4eff = HALF*c.e3/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,c.e4,e4eff,param,
                  rank-n+3,3,c.p,
                  numexp);

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-3; ++i) {
    const int nden = c.cp[i];
    exDenL(amu, e.e4, e4eff, param,
           V[nden], m2[nden], c.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby(numexp, rank-n+1+3, denc);
  }

  // store the coefficients
  c.c[0] = HALF*(c.c[0] + numexp[jext0]);
  if (lin)
    c.c[1] = numexp[jext1]/e.mp34();
  if (quad) {
    c.c[2] = numexp[jext2]/e.mp34()/e.mp34();
    c.c[7] = HALF*(c.c[7] + numexp[jext0mu2]);
  }
  if (cub) {
    c.c[3] = numexp[jext3]/e.mp34()/e.mp34()/e.mp34();
    c.c[8] = numexp[jext1mu2]/e.mp34();
  }
#ifdef NINJA_IMPLEMENTING_X1RANK
  c.c[12] = numexp[jext4]/e.mp34()/e.mp34()/e.mp34()/e.mp34();
  c.c[10] = numexp[jext2mu2]/e.mp34()/e.mp34();
  c.c[14] = HALF*(c.c[14] + numexp[jext0mu4]);
#endif

}



/////////////
// bubbles //
/////////////

template<typename MassType>
void Amplitude<MassType>::evaluateBubbles(Numerator & num,
                                          const CutsVector<Triangle> & c,
                                          CutsVector<Bubble> & b)
{

  // loop over Partitions
  CutsVector<Bubble>::iterator this_cut = b.begin();
  for (PartitionInt cut1=0; cut1<n-2+1;++cut1)
    for (PartitionInt cut2=cut1+1; cut2<n; ++cut2) {

      // Partition p and complementary partition cp
      (*this_cut).p[0] = cut1;
      (*this_cut).p[1] = cut2;
      complementaryPartition((*this_cut).p, 2, n, (*this_cut).cp);

      // Skip this bubble if the complete reconstruction is not needed
      // and both the internal propagators and the external leg are
      // massless
      if (!(Options::test & Test::GLOBAL)
          && m2[cut1] == ZERO
          && m2[cut2] == ZERO
          && taxicab_norm(real(s_mat(cut2,cut1)))
          < INFRARED_EPS) {
        ++this_cut;
        continue;
      }

      // evaluate
      evaluateBubble(num, c, *this_cut);

      // move to next cut
      ++this_cut;

    }
}


template<typename MassType>
void Amplitude<MassType>::evaluateBubble(Numerator & num,
                                         const CutsVector<Triangle> & c,
                                         Bubble & b)
{

  const PartitionInt cut1 = b.p[0];
  const PartitionInt cut2 = b.p[1];

  // check whether linear and quadratic terms are present
  const int rminusn = rank-n;
  const bool lin = rminusn >= -1;
  const bool quad = rminusn >= 0;
  const bool spurious_terms_needed = (m2[cut1]!=ZERO) || (m2[cut2]!=ZERO)
    || (Options::test & (Test::GLOBAL | Test::LOCAL_2 | Test::LOCAL_1))
    || (Options::verb & (Verbose::C2 | Verbose::C1));

  // computing external momenta
  RealMomentum p[2] = {V[cut2]-V[cut1],
                       V[cut1]-V[cut2]};

  // Get basis and other relevant momenta
  Basis e(p[1]);
  b.e2 = e.e2; b.e3 = e.e3;  b.e4 = e.e4;
  b.V0 = V[cut1];

  // get the parametric solutions for the loop momentum
  Complex param[3];
  ComplexMomentum e1effl, e2eff;
  CutBubble<MassType>(p[1], e,m2[cut1], m2[cut2]
                     ).getLoopMomentum(e1effl, e2eff, param);
  ComplexMomentum e1eff = e1effl - b.V0; // a = a - p0

  // numexp[] stores the coefficients of the numerator expansion in
  // the order:
  // {t^rank, t^(rank-1), , t^(rank-1)*x,
  //  t^(rank-2), t^(rank-2)*mu^2, t^(rank-2)*x, t^(rank-2)*x^2,
  //  t^(rank-3), t^(rank-3)*mu^2, t^(rank-3)*x, t^(rank-3)*x*mu^2,
  //  ... }
  Complex numexp[7+6*max_hr];

  // numexp[jext`i'x`j'mu`k'] gets the t^`i'*x^`j'*mu^`k' term in the
  // expansion
#ifndef NINJA_IMPLEMENTING_X1RANK
  // case: rank <= n
  const int jext2 = rank-n;
  const int jext1 = rank-n+1;
  const int jext1x1 = 2; // when != 0
  const int jext0 = rank-n+2 + (quad ? 1 : 0);
  const int jext0mu2 = 4; // when != 0
  const int jext0x1 = 2 + (quad ? 3 : 0);
  const int jext0x2 = 6; // when != 0
#else
  // case: rank == n+1
  const int jext3 = 0;
  const int jext2 = 1;
  const int jext2x1 = 2;
  const int jext1 = 3;
  const int jext1mu2 = 4;
  const int jext1x1 = 5;
  const int jext1x2 = 6;
  const int jext0 = 7;
  const int jext0mu2 = 8;
  const int jext0x1 = 9;
  const int jext0x1mu2 = 10;
  const int jext0x2 = 11;
  const int jext0x3 = 12;
#endif

  ComplexMomentum e4eff = HALF*b.e4/e.mp34();

  // compute Laurent expansion of the numerator
  num.t2Expansion(e1eff,e2eff,b.e3,e4eff,param,
                  rank-n+2,2,b.p,numexp);

  DenExp<2> denc;

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-2; ++i) {
    int nden = b.cp[i];
    exDenL(e1eff,e2eff,b.e3,e4eff,param,
           V[nden], m2[nden], b.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby(numexp, rminusn+2+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles for which this bubble is a subpartition
  PartitionInt unc; // labels the uncut denomiator of the triangle
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition2of3(b.p, (*cut3).p, &unc)) {
      RealMomentum kk = V[unc]-b.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correctbubcoeffs(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                       e1effl, e2eff, b.e3, e4eff, param,
                       kk,f,rminusn, true);
    }

  // store the coefficients
  b.c[0] = numexp[jext0];
  if (lin) {
    b.c[1] = numexp[jext0x1]/e.mp12();
    b.c[5] = numexp[jext1]/e.mp34();
  }
  if (quad) {
    b.c[9] = numexp[jext0mu2];
    b.c[2] = numexp[jext0x2]/e.mp12()/e.mp12();
    b.c[6] = numexp[jext2]/e.mp34()/e.mp34();
    b.c[8] = numexp[jext1x1]/e.mp12()/e.mp34();
  }
#ifdef NINJA_IMPLEMENTING_X1RANK
  b.c[10] = numexp[jext0x1mu2]/e.mp12();
  b.c[12] = numexp[jext1mu2]/e.mp34();
  b.c[13] = numexp[jext0x3]/e.mp12()/e.mp12()/e.mp12();
  b.c[15] = numexp[jext3]/e.mp34()/e.mp34()/e.mp34();
  b.c[17] = numexp[jext1x2]/e.mp34()/e.mp12()/e.mp12();
  b.c[19] = numexp[jext2x1]/e.mp34()/e.mp34()/e.mp12();
#endif

  if (lin && spurious_terms_needed) {

    // further expansions are needed only if linear terms are present
    // AND the spurious terms are needed (either as subtraction terms,
    // for tests, or for printing)

    // swap e3 and e4 and repeat

    e4eff = HALF*b.e3/e.mp34();

    // compute Laurent expansion of the numerator
    num.t2Expansion(e1eff,e2eff,b.e4,e4eff,param,
                    rank-n+2-1,2,b.p,numexp);

    // divide by Laurent expansion of uncut denominators
    for (int i =0; i<n-2; ++i) {
      int nden = b.cp[i];
      exDenL(e1eff,e2eff,b.e4,e4eff,param,
             V[nden], m2[nden], b.V0, m2[cut1],
             denc);
      if (!stability_check(denc.d0)) return;
      divpolyby(numexp, rminusn+2, denc);
    }

    // Compute corrections at the coefficient level, from all the
    // triangles for which this bubble is a subpartition
    PartitionInt unc; // labels the uncut denomiator of the triangle
    for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
      if(isSubPartition2of3(b.p, (*cut3).p, &unc)) {
        RealMomentum kk = V[unc]-b.V0;
        Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
        correctbubcoeffs(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                         e1effl, e2eff, b.e4, e4eff, param,
                         kk,f,rminusn, false);
      }

    // store the coefficients
    b.c[3] = numexp[jext1]/e.mp34();
    if (quad) {
      b.c[4] = numexp[jext2]/e.mp34()/e.mp34();
      b.c[7] = numexp[jext1x1]/e.mp12()/e.mp34();
    }
#ifdef NINJA_IMPLEMENTING_X1RANK
    b.c[11] = numexp[jext1mu2]/e.mp34();
    b.c[14] = numexp[jext3]/e.mp34()/e.mp34()/e.mp34();
    b.c[16] = numexp[jext1x2]/e.mp34()/e.mp12()/e.mp12();
    b.c[18] = numexp[jext2x1]/e.mp34()/e.mp34()/e.mp12();
#endif

  } // if(lin)

}



//////////////
// tadpoles //
//////////////

#ifndef NINJA_IMPLEMENTING_X1RANK

template<typename MassType>
void Amplitude<MassType>::evaluateTadpoles(Numerator & num,
                                           const CutsVector<Triangle> & c,
                                           const CutsVector<Bubble> & b,
                                           CutsVector<Tadpole> & a)
{
  // loop over Partitions
  CutsVector<Tadpole>::iterator this_cut = a.begin();
  for (PartitionInt cut1=0; cut1<n-1+1; ++cut1) {

    // Partition p and complementary partition cp
    (*this_cut).p[0] = cut1;
    complementaryPartition((*this_cut).p, 1, n, (*this_cut).cp);

    // Skip this tadpole if the loop propagator is massless
    if (m2[cut1]==ZERO) {
      ++this_cut;
      continue;
    }

    // evaluate
    evaluateTadpole(num, c, b, *this_cut);

    // move to next cut
    ++this_cut;

  }
}


template<typename MassType>
void Amplitude<MassType>::evaluateTadpole(Numerator & num,
                                          const CutsVector<Triangle> & c,
                                          const CutsVector<Bubble> & b,
                                          Tadpole & a)
{

  const PartitionInt cut1 = a.p[0];

  int rminusn =  rank-n;
  const Basis e = tadpole_basis(V,cut1,n);

  // Store the relevant vectors
  a.e1 = e.e1;
  a.e2 = e.e2;
  a.e3 = e.e3;
  a.e4 = e.e4;
  a.V0 = V[cut1];

  // get the parametric solutions for the loop momentum
  Complex param = toCmplx(m2[cut1]);
  ComplexMomentum amu = ComplexMomentum(-a.V0); // a = - p0

  // numexp[] stores the coefficients of the numerator expansion in
  // the order: {t^rank, t^(rank-1)}
  Complex numexp[2];

  // numexp[jex`i'] gets the t^`i' term in the expansion
  const int jext0=1+rminusn;

  ComplexMomentum e4eff = HALF*a.e4/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e3,e4eff,param,
                  rank-n+1,1,a.p,
                  numexp);

  // Uncut denoms
  DenExp<3> denc;

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e3, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  PartitionInt unct[2]; // uncut denominators of the triangle
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffs(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                         a.e3, kk0,f0,kk1,f1,rminusn);
    }

  // Corrections from bubbles
  PartitionInt unc; // uncut denominator of the bubble
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffs(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                       kbol, (*cut2).c, a.e3, kk,f,rminusn);
    }

  // store the coefficients
  a.c[0] = numexp[jext0];  // c0 (+ c15 e34^2)

}


// Full Tadpoles

template<typename MassType>
void Amplitude<MassType>::evaluateFullTadpoles(Numerator & num,
                                               const CutsVector<Triangle> & c,
                                               const CutsVector<Bubble> & b,
                                               CutsVector<Tadpole> & a)
{
  // loop over Partitions
  CutsVector<Tadpole>::iterator this_cut = a.begin();
  for (PartitionInt cut1=0; cut1<n-1+1;++cut1) {

    // Partition p and complementary partition cp
    (*this_cut).p[0] = cut1;
    complementaryPartition((*this_cut).p, 1, n, (*this_cut).cp);

    // Skip this tadpole if the loop propagator is massless
    if (!(Options::test & Test::GLOBAL) && m2[cut1]==ZERO) {
      ++this_cut;
      continue;
    }

    // evaluate
    evaluateFullTadpole(num, c, b, *this_cut);

    // move to next cut
    ++this_cut;

  }
}


template<typename MassType>
void Amplitude<MassType>::evaluateFullTadpole(Numerator & num,
                                              const CutsVector<Triangle> & c,
                                              const CutsVector<Bubble> & b,
                                              Tadpole & a)
{
  PartitionInt cut1 = a.p[0];

  int rminusn =  rank-n;

  // check if linear terms are present
  bool lin = rminusn >= 0;
  const Basis e = tadpole_basis(V,cut1,n);

  // Store the relevant vectors
  a.e1 = e.e1;
  a.e2 = e.e2;
  a.e3 = e.e3;
  a.e4 = e.e4;
  a.V0 = V[cut1];

  // get the parametric solutions for the loop momentum
  Complex param = toCmplx(m2[cut1]);
  ComplexMomentum amu = ComplexMomentum(-a.V0); // a = - p0

  // numexp[] stores the coefficients of the numerator expansion in
  // the order: {t^rank, t^(rank-1)}
  Complex numexp[2];

  // numexp[jex`i'] gets the t^`i' term in the expansion
  const int jext0=1+rminusn, jext1=0+rminusn;

  ComplexMomentum e4eff = HALF*a.e4/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e3,e4eff,param,
                  jext0,1,a.p,
                  numexp);

  // Uncut denoms
  DenExp<3> denc;

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e3, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  PartitionInt unct[2]; // uncut denominators of the triangle
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                           a.e3, kk0,f0,kk1,f1,rminusn);
    }

  // Corrections from bubbles
  PartitionInt unc; // uncut denominator of the bubble
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                           kbol, (*cut2).c, a.e3, kk,f,rminusn);
    }

  // store the coefficients
  a.c[0] = numexp[jext0];  // c0 (+ c15 e34^2)
  if (lin) a.c[4] = numexp[jext1]/e.mp34();

  if (lin) {

    // further expansions are needed only if linear terms are present

    // swap e3 and e4 and repeat

    e4eff = HALF*a.e3/e.mp34();

    // compute Laurent expansion of the numerator
    num.t3Expansion(amu,a.e4,e4eff,param,
                    jext1,1,a.p,
                    numexp);

    // divide by Laurent expansion of uncut denominators
    for (int i =0; i<n-1; ++i) {
      const int nden = a.cp[i];
      exDenL(amu, e.e4, e4eff, param,
             V[nden], m2[nden], a.V0, m2[cut1],
             denc);
      if (!stability_check(denc.d0)) return;
      divpolyby<3>(numexp, rank-n+1, denc);
    }

    // Compute corrections at the coefficient level, from all the
    // triangles and bubbles for which this bubble is a subpartition

    // Corrections from triangles
    PartitionInt unct[2]; // uncut denominators of the triangle
    for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
      if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
        RealMomentum kk0 = V[unct[0]]-a.V0;
        Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
        RealMomentum kk1 = V[unct[1]]-a.V0;
        Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
        correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                             a.e4, kk0,f0,kk1,f1,rminusn);
      }

    // Corrections from bubbles
    PartitionInt unc; // uncut denominator of the bubble
    for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
      if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
        RealMomentum kk = V[unc]-a.V0;
        RealMomentum kbol = (*cut2).V0-a.V0;
        Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
        correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                             kbol, (*cut2).c, a.e4, kk,f,rminusn);
      }

    // store the coefficients
    a.c[3] = numexp[jext1]/e.mp34();


    // swap (e3,e4) <-> (e1,e2) and repeat

    e4eff = HALF*a.e2/e.mp12();

    // compute Laurent expansion of the numerator
    num.t3Expansion(amu,a.e1,e4eff,param,
                    jext1,1,a.p,
                    numexp);

    // divide by Laurent expansion of uncut denominators
    for (int i =0; i<n-1; ++i) {
      const int nden = a.cp[i];
      exDenL(amu, e.e1, e4eff, param,
             V[nden], m2[nden], a.V0, m2[cut1],
             denc);
      if (!stability_check(denc.d0)) return;
      divpolyby<3>(numexp, rank-n+1, denc);
    }

    // Compute corrections at the coefficient level, from all the
    // triangles and bubbles for which this bubble is a subpartition

    // Corrections from triangles
    for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
      if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
        RealMomentum kk0 = V[unct[0]]-a.V0;
        Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
        RealMomentum kk1 = V[unct[1]]-a.V0;
        Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
        correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                             a.e1, kk0,f0,kk1,f1,rminusn);
      }

    // Corrections from bubbles
    for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
      if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
        RealMomentum kk = V[unc]-a.V0;
        RealMomentum kbol = (*cut2).V0-a.V0;
        Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
        correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                             kbol, (*cut2).c, a.e1, kk,f,rminusn);
      }

    // store the coefficients
    a.c[2] = numexp[jext1]/e.mp12();


    // swap e1 and e2 and repeat

    e4eff = HALF*a.e1/e.mp12();

    // compute Laurent expansion of the numerator
    num.t3Expansion(amu,a.e2,e4eff,param,
                    jext1,1,a.p,
                    numexp);

    // divide by Laurent expansion of uncut denominators
    for (int i =0; i<n-1; ++i) {
      const int nden = a.cp[i];
      exDenL(amu, e.e2, e4eff, param,
             V[nden], m2[nden], a.V0, m2[cut1],
             denc);
      if (!stability_check(denc.d0)) return;
      divpolyby<3>(numexp, rank-n+1, denc);
    }

    // Compute corrections at the coefficient level, from all the
    // triangles and bubbles for which this bubble is a subpartition

    // Corrections from triangles
    for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
      if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
        RealMomentum kk0 = V[unct[0]]-a.V0;
        Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
        RealMomentum kk1 = V[unct[1]]-a.V0;
        Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
        correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                             a.e2, kk0,f0,kk1,f1,rminusn);
      }

    // Corrections from bubbles
    for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
      if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
        RealMomentum kk = V[unc]-a.V0;
        RealMomentum kbol = (*cut2).V0-a.V0;
        Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
        correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                             kbol, (*cut2).c, a.e2, kk,f,rminusn);
      }

    // store the coefficients
    a.c[1] = numexp[jext1]/e.mp12();

  } // if (lin)
}


#else // ! NINJA_IMPLEMENTING_X1RANK


template<typename MassType>
void Amplitude<MassType>::evaluateTadpoles(Numerator & num,
                                           const CutsVector<Triangle> & c,
                                           const CutsVector<Bubble> & b,
                                           CutsVector<Tadpole> & a)
{
  // loop over Partitions
  CutsVector<Tadpole>::iterator this_cut = a.begin();
  for (PartitionInt cut1=0; cut1<n-1+1; ++cut1) {

    // Partition p and complementary partition cp
    (*this_cut).p[0] = cut1;
    complementaryPartition((*this_cut).p, 1, n, (*this_cut).cp);

    // Skip this tadpole if the loop propagator is massless
    if (m2[cut1]==ZERO) {
      ++this_cut;
      continue;
    }

    // evaluate
    evaluateTadpole(num, c, b, *this_cut);

    // move to next cut
    ++this_cut;

  }
}


template<typename MassType>
void Amplitude<MassType>::evaluateTadpole(Numerator & num,
                                          const CutsVector<Triangle> & c,
                                          const CutsVector<Bubble> & b,
                                          Tadpole & a)
{
  const PartitionInt cut1 = a.p[0];
  const Basis e = tadpole_basis(V,cut1,n);

  // Store the relevant vectors
  a.e1 = e.e1;
  a.e2 = e.e2;
  a.e3 = e.e3;
  a.e4 = e.e4;
  a.V0 = V[cut1];

  // get the parametric solutions for the loop momentum
  Complex param = toCmplx(m2[cut1]);
  ComplexMomentum amu = ComplexMomentum(-a.V0); // a = - p0

  // numexp[] stores the coefficients of the numerator expansion in
  // the order: {t^rank, t^(rank-1)}
  Complex numexp[4];

  // numexp[jex`i'] gets the t^`i' term in the expansion
  //const int jext2 = 0;
  //const int jext1 = 1;
  const int jext0 = 2;
  const int jext0mu2 = 3;

  ComplexMomentum e4eff = HALF*a.e2/e.mp12();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e1,e4eff,param,
                  rank-n+1,1,a.p,
                  numexp);

  // Uncut denoms
  DenExp<3> denc;

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e1, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  PartitionInt unct[2]; // uncut denominators of the triangle
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffs(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                         a.e1, e4eff, param, kk0,f0,kk1,f1,
                       true);
    }

  // Corrections from bubbles
  PartitionInt unc; // uncut denominator of the bubble
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffs(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                       kbol, (*cut2).c,
                       a.e1, e4eff, param, kk,f,
                       true);
    }

  // store the coefficients
  a.c[0] = numexp[jext0];
  a.c[14] = numexp[jext0mu2];


  // second expansion

  e4eff = HALF*a.e4/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e3,e4eff,param,
                  rank-n+1,1,a.p,
                  numexp);

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e3, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffs(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                         a.e3, e4eff, param, kk0,f0,kk1,f1,
                       true);
    }

  // Corrections from bubbles
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffs(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                       kbol, (*cut2).c,
                       a.e3, e4eff, param, kk,f,
                       true);
    }

  // store the coefficients
  a.c[15] = TWO*(numexp[jext0mu2] - a.c[14])/e.mp34();
}


// Full tadpoles

template<typename MassType>
void Amplitude<MassType>::evaluateFullTadpoles(Numerator & num,
                                               const CutsVector<Triangle> & c,
                                               const CutsVector<Bubble> & b,
                                               CutsVector<Tadpole> & a)
{
  // loop over Partitions
  CutsVector<Tadpole>::iterator this_cut = a.begin();
  for (PartitionInt cut1=0; cut1<n-1+1; ++cut1) {

    // Partition p and complementary partition cp
    (*this_cut).p[0] = cut1;
    complementaryPartition((*this_cut).p, 1, n, (*this_cut).cp);

    // Skip this tadpole if the loop propagator is massless
    if (!(Options::test & Test::GLOBAL) && m2[cut1]==ZERO) {
      ++this_cut;
      continue;
    }

    // evaluate
    evaluateFullTadpole(num, c, b, *this_cut);

    // move to next cut
    ++this_cut;

  }
}


template<typename MassType>
void Amplitude<MassType>::evaluateFullTadpole(Numerator & num,
                                              const CutsVector<Triangle> & c,
                                              const CutsVector<Bubble> & b,
                                              Tadpole & a)
{
  const PartitionInt cut1 = a.p[0];
  const Basis e = tadpole_basis(V,cut1,n);

  // Store the relevant vectors
  a.e1 = e.e1;
  a.e2 = e.e2;
  a.e3 = e.e3;
  a.e4 = e.e4;
  a.V0 = V[cut1];

  // get the parametric solutions for the loop momentum
  Complex param = toCmplx(m2[cut1]);
  ComplexMomentum amu = ComplexMomentum(-a.V0); // a = - p0

  // numexp[] stores the coefficients of the numerator expansion in
  // the order: {t^rank, t^(rank-1),...} (like triangles)
  Complex numexp[4];

  // numexp[jex`i'] gets the t^`i' term in the expansion
  const int jext2 = 0;
  const int jext1 = 1;
  const int jext0 = 2;
  const int jext0mu2 = 3;

  ComplexMomentum e4eff = HALF*a.e2/e.mp12();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e1,e4eff,param,
                  rank-n+1,1,a.p,
                  numexp);

  // Uncut denoms
  DenExp<3> denc;

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e1, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  PartitionInt unct[2]; // uncut denominators of the triangle
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                           a.e1, e4eff, param, kk0,f0,kk1,f1,
                           true);
    }

  // Corrections from bubbles
  PartitionInt unc; // uncut denominator of the bubble
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                           kbol, (*cut2).c,
                           a.e1, e4eff, param, kk,f,
                           true);
    }

  // store the coefficients
  a.c[0] = numexp[jext0];
  a.c[2] = numexp[jext1]/e.mp12();
  a.c[6] = numexp[jext2]/e.mp12()/e.mp12();
  a.c[14] = numexp[jext0mu2];


  // second expansion

  e4eff = HALF*a.e4/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e3,e4eff,param,
                  rank-n+1,1,a.p,
                  numexp);

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e3, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                           a.e3, e4eff, param, kk0,f0,kk1,f1,
                           true);
    }

  // Corrections from bubbles
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                           kbol, (*cut2).c,
                           a.e3, e4eff, param, kk,f,
                           true);
    }

  // store the coefficients
  a.c[15] = TWO*(numexp[jext0mu2] - a.c[14])/e.mp34();
  a.c[4] = numexp[jext1]/e.mp34();
  a.c[8] = numexp[jext2]/e.mp34()/e.mp34();

  // swap e3 and e4 and repeat

  e4eff = HALF*a.e3/e.mp34();

  // compute Laurent expansion of the numerator
  num.t3Expansion(amu,a.e4,e4eff,param,
                  rank-n,1,a.p,
                  numexp);

  // divide by Laurent expansion of uncut denominators
  for (int i =0; i<n-1; ++i) {
    const int nden = a.cp[i];
    exDenL(amu, e.e4, e4eff, param,
           V[nden], m2[nden], a.V0, m2[cut1],
           denc);
    if (!stability_check(denc.d0)) return;
    divpolyby<3>(numexp, rank-n+1, denc);
  }

  // Compute corrections at the coefficient level, from all the
  // triangles and bubbles for which this bubble is a subpartition

  // Corrections from triangles
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3)
    if (isSubPartition1of3(a.p,(*cut3).p,unct)) {
      RealMomentum kk0 = V[unct[0]]-a.V0;
      Complex f0 = mp2(kk0)+(m2[cut1]-m2[unct[0]]);
      RealMomentum kk1 = V[unct[1]]-a.V0;
      Complex f1 = mp2(kk1)+(m2[cut1]-m2[unct[1]]);
      correcttadcoeffsfull(numexp, (*cut3).e3, (*cut3).e4, (*cut3).c,
                           a.e4, e4eff, param, kk0,f0,kk1,f1,
                           true);
    }

  // Corrections from bubbles
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2)
    if (isSubPartition1of2(a.p, (*cut2).p, &unc)) {
      RealMomentum kk = V[unc]-a.V0;
      RealMomentum kbol = (*cut2).V0-a.V0;
      Complex f = mp2(kk)+(m2[cut1]-m2[unc]);
      correcttadcoeffsfull(numexp, (*cut2).e2, (*cut2).e3, (*cut2).e4,
                           kbol, (*cut2).c,
                           a.e4, e4eff, param, kk,f,
                           true);
    }

  // store the coefficients
  a.c[3] = numexp[jext1]/e.mp34();
  a.c[7] = numexp[jext2]/e.mp34()/e.mp34();

  // NOTE: We won't compute all teh coefficients for higher-rank
  // tadpoles, because it would require several other expansions.
  // These are however enough to give the possibility of making a
  // local test on a particular branch of the solutions.
}


#endif // NINJA_IMPLEMENTING_X1RANK



///////////////////////
// N = N test global //
///////////////////////

template<typename MassType>
void Amplitude<MassType>::NeqNtest(Numerator & num,
                                  const CutsVector<Pentagon> & pen,
                                  const CutsVector<Box> & d,
                                  const CutsVector<Triangle> & c,
                                  const CutsVector<Bubble> & b,
                                  const CutsVector<Tadpole> & a,
                                  const ComplexMomentum & q,
                                  const Complex & muq)
{
  Complex N = num.evaluate(q, muq, 0, NULL);
  Complex subtr = ZERO;
  // loop over the quintuple cuts to determine the subtraction terms
  for (unsigned int cut5 = 0; cut5<pen.size(); ++cut5) {
    Complex res5 = pen[cut5].poly(q,muq);
    Complex D = ONE;
    for (int i =0;i<n-5;++i)
      D *= Den(q, V[ pen[cut5].cp[i] ], m2[ pen[cut5].cp[i] ], muq);
    subtr += res5*D;
  }
  // loop over the quadruple cuts to determine the subtraction terms
  for (unsigned int cut4 = 0; cut4<d.size(); ++cut4) {
    Complex res4 = d[cut4].poly(q,muq);
    Complex D = ONE;
    for (int i =0;i<n-4;++i)
      D *= Den(q, V[ d[cut4].cp[i] ], m2[ d[cut4].cp[i] ], muq);
    subtr += res4*D;
  }
  // loop over the triple cuts to determine the subtraction terms
  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3) {
    Complex res3 = (*cut3).poly(q,muq);
    Complex D = ONE;
    for (int i =0;i<n-3;++i)
      D *= Den(q, V[ (*cut3).cp[i] ], m2[ (*cut3).cp[i] ], muq);
    subtr += res3*D;
  }
  // loop over the double cuts to determine the subtraction terms
  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2) {
    Complex res2 = (*cut2).poly(q,muq);
    Complex D = ONE;
    for (int i =0;i<n-2;++i)
      D *= Den(q, V[ (*cut2).cp[i] ], m2[ (*cut2).cp[i] ], muq);
    subtr += res2*D;
  }
  // loop over the double cuts to determine the subtraction terms
  for (unsigned int cut1 = 0; cut1<a.size(); ++cut1) {
    Complex res1 = a[cut1].poly(q,muq);
    Complex D = ONE;
    for (int i =0;i<n-1;++i)
      D *= Den(q, V[ a[cut1].cp[i] ], m2[ a[cut1].cp[i] ], muq);
    subtr += res1*D;
  }
  // check the correctness of the reconstruction
  if (Options::verb & Verbose::GLOBAL_TEST) {
    (*Options::out) << "0 -- N = N test global at q = " << q
                    << " and mu^2 = " << muq << " : " << endl;
    (*Options::out) << "The value of the numerator is  :  " << N << endl;
    (*Options::out) << "The reconstructed numerator is :  " << subtr << endl;
    (*Options::out) << "The relative error is          :  " << (N-subtr)/N << endl;
    (*Options::out) << endl;
  }
  if (abs((N-subtr)/N) > Options::test_tol && abs(N)>1.0e-8)
    return_val = Amplitude::TEST_FAILED | return_val;
}

//////////////////////
// N = N test local //
//////////////////////

template<typename MassType>
void Amplitude<MassType>::local4NeqNtests(Numerator & num,
                                         const CutsVector<Pentagon> & pen,
                                         const CutsVector<Box> & d)
{
  int ret = Amplitude::SUCCESS;
  bool verbose = Options::verb & Verbose::LOCAL_TEST_4;

  if (verbose)
    (*Options::out) << endl;

  for (unsigned int cut4 = 0; cut4<d.size(); ++cut4) {
    int CUT1 = d[cut4].p[0], CUT2 = d[cut4].p[1];
    int CUT3 = d[cut4].p[2], CUT4 = d[cut4].p[3];

    if (verbose) {
      (*Options::out) << "N = N test local on Quadruple Cut : "
                      << CUT1 << "," << CUT2 << "," << CUT3
                      << "," << CUT4 << endl;
    }

    // computing external momenta
    RealMomentum p[4] = {V[CUT2]-V[CUT1],
                         V[CUT3]-V[CUT2],
                         V[CUT4]-V[CUT3],
                         V[CUT1]-V[CUT4]};
    // Get basis
    Basis e(p[3],p[0]);
    // Choose mu^2
    Real muq = 1.4;
    // get the two solutions for the loop momenta
    ComplexMomentum l[2];
    CutBox<MU_MASS_TYPE>(p[3], p[0], p[1], e,
                         m2[CUT1]+muq, m2[CUT2]+muq,
                         m2[CUT3]+muq, m2[CUT4]+muq
                        ).getLoopMomentum(l[0], l[1]);
    ComplexMomentum q = l[0] - d[cut4].V0;

    Complex N = num.evaluate(q, muq, 0, NULL);

    Complex subtr = ZERO;
    // loop over the quintuple cuts to determine the subtraction terms
    for (unsigned int cut5 = 0; cut5<pen.size(); ++cut5) {
      Complex res5 = pen[cut5].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-5;++i)
        D *= Den(q, V[ pen[cut5].cp[i] ], m2[ pen[cut5].cp[i] ], muq);
      subtr += res5*D;
    }
    // loop over the quadruple cuts to determine the subtraction terms
    {
      Complex res4 = d[cut4].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[ d[cut4].cp[i] ], m2[ d[cut4].cp[i] ], muq);
      subtr += res4*D;
    }
    // check the correctness of the reconstruction
    if (verbose) {
      (*Options::out) << "N = N test at q = " << q
                << " and mu^2 = " << muq << " : " << endl;
      (*Options::out) << "The value of the numerator is  :  " << N << endl;
      (*Options::out) << "The reconstructed numerator is :  " << subtr << endl;
      (*Options::out) << "The relative error is          :  " << (N-subtr)/N << endl;
      if (!(abs(N)>1.0e-8))
        (*Options::out) << "Note: small numerator -> test won't fail" << endl;
      (*Options::out) << endl;
    }
    if (abs((N-subtr)/N) > Options::test_tol && abs(N)>1.0e-8)
      ret = Amplitude::TEST_FAILED;
  }
  if (verbose)
    (*Options::out) << endl;
  return_val = ret | return_val;
}

template<typename MassType>
void Amplitude<MassType>::local3NeqNtests(Numerator & num,
                                         const CutsVector<Pentagon> & pen,
                                         const CutsVector<Box> & d,
                                         const CutsVector<Triangle> & c)
{
  int ret = Amplitude::SUCCESS;
  bool verbose = Options::verb & Verbose::LOCAL_TEST_3;

  if (verbose)
    (*Options::out) << endl;

  for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3) {

    int CUT1 = (*cut3).p[0], CUT2 = (*cut3).p[1], CUT3 = (*cut3).p[2];

    if (verbose) {
      (*Options::out) << "N = N test local on Triple Cut : "
                      << CUT1 << "," << CUT2 << "," << CUT3 << endl;
    }

    // Choose a value of q on the cut

    // Chosing t and mu^2
    Real t = 12.3;
    //Real muq = 0.5;
    Real muq = 1.3;
    // computing external momenta
    RealMomentum p[3] = {V[CUT2]-V[CUT1],
                         V[CUT3]-V[CUT2],
                         V[CUT1]-V[CUT3]};
    // Get basis
    Basis e(p[2],p[0]);
    // get the parametric solutions for the loop momentum
    Complex param;
    ComplexMomentum amu;
    CutTriangle<MassType>(p[2], p[0], e,
                              m2[CUT1] , m2[CUT2], m2[CUT3]
                             ).getLoopMomentum(amu, param);
    amu = amu - (*cut3).V0; // a = a - p0
    ComplexMomentum e4eff = HALF*e.e4/e.mp34();
    ComplexMomentum q = amu + t*e.e3 + (param+muq)*e4eff/t ;

    // Evaluate the numerator
    Complex N = num.evaluate(q, muq, 0, NULL);

    // Compute subtraction terms

    Complex subtr = ZERO;
    // loop over the quintuple cuts to determine the subtraction terms
    for (unsigned int cut5 = 0; cut5<pen.size(); ++cut5) {
      Complex res5 = pen[cut5].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-5;++i)
        D *= Den(q, V[ pen[cut5].cp[i] ], m2[ pen[cut5].cp[i] ], muq);
      subtr += res5*D;
    }
    // loop over the quadruple cuts to determine the subtraction terms
    for (unsigned int cut4 = 0; cut4<d.size(); ++cut4) {
      Complex res4 = d[cut4].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[ d[cut4].cp[i] ], m2[ d[cut4].cp[i] ], muq);
      subtr += res4*D;
    }
    {
      Complex res3 = (*cut3).poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-3;++i)
        D *= Den(q, V[ (*cut3).cp[i] ], m2[ (*cut3).cp[i] ], muq);
      subtr += res3*D;
    }
    // check the correctness of the reconstruction
    if (verbose) {
      (*Options::out) << "N = N test at q = " << q << " and mu^2 = "
                << muq << " : " << endl;
      (*Options::out) << "The value of the numerator is  :  " << N << endl;
      (*Options::out) << "The reconstructed numerator is :  " << subtr << endl;
      (*Options::out) << "The relative error is          :  " << (N-subtr)/N << endl;
      if (!(abs(N)>1.0e-8))
        (*Options::out) << "Note: small numerator -> test won't fail" << endl;
      (*Options::out) << endl;
    }
    if (abs((N-subtr)/N) > Options::test_tol && abs(N)>1.0e-8)
      ret = Amplitude::TEST_FAILED;
  }
  if (verbose)
    (*Options::out) << endl;
  return_val = ret | return_val;
}

template<typename MassType>
void Amplitude<MassType>::local2NeqNtests(Numerator & num,
                                         const CutsVector<Pentagon> & pen,
                                         const CutsVector<Box> & d,
                                         const CutsVector<Triangle> & c,
                                         const CutsVector<Bubble> & b)
{
  int ret = Amplitude::SUCCESS;
  bool verbose = Options::verb & Verbose::LOCAL_TEST_2;

  if (verbose)
    (*Options::out) << endl;

  for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2) {

    int CUT1 = (*cut2).p[0], CUT2 = (*cut2).p[1];

    if (!(Options::test & Test::GLOBAL)
        && m2[CUT1] == ZERO
        && m2[CUT2] == ZERO
        && taxicab_norm(real(s_mat(CUT2,CUT1)))
        < INFRARED_EPS) {
      continue;
    }

    if (verbose) {
      (*Options::out) << "N = N test local on Double Cut : "
                      << CUT1 << "," << CUT2 << endl;
    }

    // Choose a value of q on the cut

    // Chosing x0, t and mu^2
    Real x0 = 1., t = 2.;
    Real muq = 1.2;
    // computing external momenta
    RealMomentum p[2] = {V[CUT2]-V[CUT1],
                         V[CUT1]-V[CUT2]};
    // Get basis
    Basis e(p[1]);
    // get the parametric solutions for the loop momentum
    ComplexMomentum e1effl, e2eff;
    Complex param[3];
    CutBubble<MassType>(p[1], e,m2[CUT1], m2[CUT2]
                        ).getLoopMomentum(e1effl, e2eff, param);
    ComplexMomentum e1eff = e1effl - cut2->V0; // a = a - p0
    ComplexMomentum e4eff = HALF*e.e3/e.mp34();
    Complex X = muq + param[0] + param[1]*x0 + param[2]*x0*x0;
    ComplexMomentum q = e1eff + x0*e2eff + t*e.e4 + X*e4eff/t ;

    // Evaluate the numerator
    Complex N = num.evaluate(q, muq, 0, NULL);

    // Compute subtraction terms

    Complex subtr = ZERO;
    // loop over the quintuple cuts to determine the subtraction terms
    for (unsigned int cut5 = 0; cut5<pen.size(); ++cut5) {
      Complex res5 = pen[cut5].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-5;++i)
        D *= Den(q, V[ pen[cut5].cp[i] ], m2[ pen[cut5].cp[i] ], muq);
      subtr += res5*D;
    }
    // loop over the quadruple cuts to determine the subtraction terms
    for (unsigned int cut4 = 0; cut4<d.size(); ++cut4) {
      Complex res4 = d[cut4].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[ d[cut4].cp[i] ], m2[ d[cut4].cp[i] ], muq);
      subtr += res4*D;
    }
    // loop over the triple cuts to determine the subtraction terms
    for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3) {
      Complex res3 = (*cut3).poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-3;++i)
        D *= Den(q, V[ (*cut3).cp[i] ], m2[ (*cut3).cp[i] ], muq);
      subtr += res3*D;
    }
    {
      Complex res2 = (*cut2).poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-2;++i)
        D *= Den(q, V[ (*cut2).cp[i] ], m2[ (*cut2).cp[i] ], muq);
      subtr += res2*D;
    }
    // check the correctness of the reconstruction
    if (verbose) {
      (*Options::out) << "N = N test at q = " << q
                      << " and mu^2 = " << muq << " : " << endl;
      (*Options::out) << "The value of the numerator is  :  " << N << endl;
      (*Options::out) << "The reconstructed numerator is :  " << subtr << endl;
      (*Options::out) << "The relative error is          :  "
                      << (N-subtr)/N << endl;
      if (!(abs(N)>1.0e-8))
        (*Options::out) << "Note: small numerator -> test won't fail" << endl;
      (*Options::out) << endl;
    }
    if (abs((N-subtr)/N) > Options::test_tol && abs(N)>1.0e-8)
      ret = Amplitude::TEST_FAILED;
  }
  if (verbose)
    (*Options::out) << endl;
  return_val = ret | return_val;
}


template<typename MassType>
void Amplitude<MassType>::local1NeqNtests(Numerator & num,
                                         const CutsVector<Pentagon> & pen,
                                         const CutsVector<Box> & d,
                                         const CutsVector<Triangle> & c,
                                         const CutsVector<Bubble> & b,
                                         const CutsVector<Tadpole> & a)
{
  int ret = Amplitude::SUCCESS;
  bool verbose = Options::verb & Verbose::LOCAL_TEST_1;

  if (verbose)
    (*Options::out) << endl;

  for (unsigned int cut1 = 0; cut1<a.size(); ++cut1) {

    if (!(Options::test & Test::GLOBAL) && m2[cut1] == ZERO) {
      continue;
    }

    if (verbose)
      (*Options::out) << "N = N test local on Single Cut : " << cut1 << endl;

    // Choose a value of q on the cut

    // Chosing x1, x2, t and mu^2
#ifndef NINJA_IMPLEMENTING_X1RANK
    Real x1 = 1.5, x2 = 1.8, t = 2.;
    Real muq = 1.1;
#else
    Real x1 = 0.0, x2 = 0.0, t = 2.;
    Real muq = 1.1;
#endif
    const Basis e = tadpole_basis(V,cut1,n);
    // get the parametric solutions for the loop momentum
    ComplexMomentum amu;
#ifndef NINJA_IMPLEMENTING_X1RANK
    // get the parametric solutions for the loop momentum
    Complex X;
    CutTadpole<MU_MASS_TYPE>(e, m2[cut1]+muq
                             ).getLoopMomentum(x1, x2, X);
    amu =  -a[cut1].V0 + x1*e.e1 + x2*e.e2;
    ComplexMomentum q = amu + t*e.e4 + X*e.e3/t;
#else
    amu =  -a[cut1].V0;
    ComplexMomentum e4eff = HALF*e.e4/e.mp34();
    (void)(x1);  (void)(x2);
    ComplexMomentum q = amu + t*e.e3 + (m2[cut1]+muq)*e4eff/t;
#endif

    // Evaluate the numerator
    Complex N = num.evaluate(q, muq, 0, NULL);

    // Compute subtraction terms

    Complex subtr = ZERO;
    // loop over the quintuple cuts to determine the subtraction terms
    for (unsigned int cut5 = 0; cut5<pen.size(); ++cut5) {
      Complex res5 = pen[cut5].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-5;++i)
        D *= Den(q, V[ pen[cut5].cp[i] ], m2[ pen[cut5].cp[i] ], muq);
      subtr += res5*D;
    }
    // loop over the quadruple cuts to determine the subtraction terms
    for (unsigned int cut4 = 0; cut4<d.size(); ++cut4) {
      Complex res4 = d[cut4].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-4;++i)
        D *= Den(q, V[ d[cut4].cp[i] ], m2[ d[cut4].cp[i] ], muq);
      subtr += res4*D;
    }
    // loop over the triple cuts to determine the subtraction terms
    for (TrianglesCIter cut3 = c.begin(); cut3!=c.end(); ++cut3) {
      Complex res3 = (*cut3).poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-3;++i)
        D *= Den(q, V[ (*cut3).cp[i] ], m2[ (*cut3).cp[i] ], muq);
      subtr += res3*D;
    }
    // loop over the double cuts to determine the subtraction terms
    for (BubblesCIter cut2 = b.begin(); cut2!=b.end(); ++cut2) {
      Complex res2 = (*cut2).poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-2;++i)
        D *= Den(q, V[ (*cut2).cp[i] ], m2[ (*cut2).cp[i] ], muq);
      subtr += res2*D;
    }
    {
      Complex res1 = a[cut1].poly(q,muq);
      Complex D = ONE;
      for (int i =0;i<n-1;++i)
        D *= Den(q, V[ a[cut1].cp[i] ], m2[ a[cut1].cp[i] ], muq);
      subtr += res1*D;
    }
    // check the correctness of the reconstruction
    if (verbose) {
      (*Options::out) << "N = N test at q = " << q
                      << " and mu^2 = " << muq << " : " << endl;
      (*Options::out) << "The value of the numerator is  :  " << N << endl;
      (*Options::out) << "The reconstructed numerator is :  " << subtr << endl;
      (*Options::out) << "The relative error is          :  " << (N-subtr)/N
                      << endl;
      if (!(abs(N)>1.0e-8))
        (*Options::out) << "Note: small numerator -> test won't fail" << endl;
      (*Options::out) << endl;
    }
    if (abs((N-subtr)/N) > Options::test_tol && abs(N)>1.0e-8)
      ret = Amplitude::TEST_FAILED;
  }
  if (verbose)
    (*Options::out) << endl;
  return_val = ret | return_val;
}


#undef MU_MASS_TYPE

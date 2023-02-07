// Definitions of the polynomial functions of each residue and the
// routines which print their coefficients.


// note: If the library is compiled with higher-rank support, this
// file is included in both cuts.cc and Xcuts.cc.  The preprocessor
// macro NINJA_IMPLEMENTING_X1RANK is defined only in the latter and
// is used to distinguish between the two implementations.

Complex Box::poly(const ComplexMomentum & q, const Complex & muq) const
{
  return c[0] + c[2]*muq + c[4]*muq*muq + (c[1] + c[3]*muq
#ifdef NINJA_IMPLEMENTING_X1RANK
                                           + c[5]*muq*muq
#endif
                                           ) * mp(q+V0,Vort);
}


Complex Triangle::poly(const ComplexMomentum & q, const Complex & muq) const
{
  Complex qe3 = mp(q+V0,e3), qe4 = mp(q+V0,e4);
  return c[0] + c[7]*muq
    + (c[1]+c[8]*muq) * qe3 + c[2]*qe3*qe3 + c[3]*qe3*qe3*qe3
    + (c[4]+c[9]*muq) * qe4 + c[5]*qe4*qe4 + c[6]*qe4*qe4*qe4
#ifdef NINJA_IMPLEMENTING_X1RANK
    + c[14]*muq*muq
    + c[10]*muq*qe3*qe3 + c[12]*qe3*qe3*qe3*qe3
    + c[11]*muq*qe4*qe4 + c[13]*qe4*qe4*qe4*qe4
#endif
    ;
}


Complex Bubble::poly(const ComplexMomentum & q, const Complex & muq) const
{
  Complex qe2 = mp(q+V0,e2), qe3 = mp(q+V0,e3),
    qe4 = mp(q+V0,e4);
  return c[0] + c[9]*muq
    + c[1]*qe2 + c[2]*qe2*qe2
    + c[3]*qe3 + c[4]*qe3*qe3
    + c[5]*qe4 + c[6]*qe4*qe4
    + c[7]*qe2*qe3 + c[8]*qe2*qe4
#ifdef NINJA_IMPLEMENTING_X1RANK
    + muq*(  c[10]*qe2 + c[11]*qe3 + c[12]*qe4 )
    + c[13]*qe2*qe2*qe2 + c[14]*qe3*qe3*qe3 + c[15]*qe4*qe4*qe4
    + c[16]*qe2*qe2*qe3 + c[17]*qe2*qe2*qe4
    + c[18]*qe2*qe3*qe3 + c[19]*qe2*qe4*qe4
#endif
    ;
}

#ifndef NINJA_IMPLEMENTING_X1RANK

Complex Tadpole::poly(const ComplexMomentum & q, const Complex &) const
{
  Complex qe1 = mp(q+V0,e1), qe2 = mp(q+V0,e2),
    qe3 = mp(q+V0,e3), qe4 = mp(q+V0,e4);
  return c[0] + c[1]*qe1 + c[2]*qe2 + c[3]*qe3 + c[4]*qe4;
}

#else //  NINJA_IMPLEMENTING_X1RANK

Complex Tadpole::poly(const ComplexMomentum & q, const Complex & muq) const
{
  Complex qe1 = mp(q+V0,e1), qe2 = mp(q+V0,e2),
    qe3 = mp(q+V0,e3), qe4 = mp(q+V0,e4);
  return c[0] + c[1]*qe1 + c[2]*qe2 + c[3]*qe3 + c[4]*qe4
    + c[5]*qe1*qe1 + c[6]*qe2*qe2 + c[7]*qe3*qe3 + c[8]*qe4*qe4
    + c[10]*qe1*qe3 + c[11]*qe1*qe4 + c[12]*qe2*qe3 + c[13]*qe2*qe4
    + c[14]*muq + c[15]*qe3*qe4 ;
}

#endif //  NINJA_IMPLEMENTING_X1RANK


// printing coefficients

void print(const CutsVector<Pentagon> & p)
{
    (*Options::out) << "5 -- Quintuple cuts:\n\n"
                    << "n. partitions = " << p.size() << "\n\n"
                    << "Coefficients:" << endl;
    for (unsigned int i = 0; i<p.size(); ++i) {
      (*Options::out) << "* partition "
                      << int(p[i].p[0]) << ", "
                      << int(p[i].p[1]) << ", "
                      << int(p[i].p[2]) << ", "
                      << int(p[i].p[3]) << ", "
                      << int(p[i].p[4]) << endl;
      (*Options::out) << "  c[0] = " << Options::chop(p[i].c[0]) << endl;
    }
}

void print(const CutsVector<Box> & d)
{
    (*Options::out) << "\n\n4 -- Quadruple cuts:\n\n"
                    << "n. partitions = " << d.size() << "\n\n"
                    << "Coefficients:" << endl;
    for (unsigned int i = 0; i<d.size(); ++i) {
      (*Options::out) << "* partition "
                      << int(d[i].p[0]) << ", "
                      << int(d[i].p[1]) << ", "
                      << int(d[i].p[2]) << ", "
                      << int(d[i].p[3]) << endl;
      for (int j =0; j < Box::csize; ++j)
        (*Options::out) << "  c[" << j << "] = " << Options::chop(d[i].c[j])
                        << endl;
      (*Options::out) << endl;
    }
}

void print(const CutsVector<Triangle> & c)
{
    (*Options::out) << "\n\n3 -- Triple cuts:\n\n"
                    << "n. partitions = " << c.size() << "\n\n"
                    << "Coefficients:" << endl;
    for (unsigned int i = 0; i<c.size(); ++i) {
      (*Options::out) << "* partition "
                      << int(c[i].p[0]) << ", "
                      << int(c[i].p[1]) << ", "
                      << int(c[i].p[2]) << endl;
      for (int j =0; j<Triangle::csize; ++j)
        (*Options::out) << "  c[" << j << "] = " << Options::chop(c[i].c[j])
                        << endl;
    }
}

void print(const CutsVector<Bubble> & b)
{
    (*Options::out) << "\n\n2 -- Double cuts:\n\n"
                    << "n. partitions = " << b.size() << "\n\n"
                    << "Coefficients:" << endl;
    for (unsigned int i = 0; i<b.size(); ++i) {
      (*Options::out) << "* partition "
                      << int(b[i].p[0]) << ", "
                      << int(b[i].p[1]) << endl;
      for (int j =0; j<Bubble::csize; ++j)
        (*Options::out) << "  c[" << j << "] = " << Options::chop(b[i].c[j])
                        << endl;
    }
}

void print(const CutsVector<Tadpole> & a)
{
    (*Options::out) << "\n\n1 -- Single cuts:\n\n"
                    << "n. partitions = " << a.size() << "\n\n"
                    << "Coefficients:" << endl;
    for (unsigned int i = 0; i<a.size(); ++i) {
      (*Options::out) << "* partition " << int(a[i].p[0]) << endl;
      for (int j =0; j<Tadpole::csize; ++j)
        (*Options::out) << "  c[" << j << "] = " << Options::chop(a[i].c[j])
                        << endl;
    }
}


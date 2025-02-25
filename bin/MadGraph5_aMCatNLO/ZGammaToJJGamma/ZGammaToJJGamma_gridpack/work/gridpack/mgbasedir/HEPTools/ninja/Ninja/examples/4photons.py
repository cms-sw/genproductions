#! /usr/bin/env python

import ninjanumgen

def main():
    mynum = ninjanumgen.DiagramExpansion('4photons.frm',
                                         '4photons_num.cc',
                                         4, rank=4,
                                         diagname='FourPhotons')
    mynum.writeSource()

if __name__ == "__main__":
    main()

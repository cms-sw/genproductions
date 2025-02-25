#! /usr/bin/env python

import ninjanumgen

def main():
    mynum = ninjanumgen.DiagramExpansion('ttbarh.frm',
                                         'ttbarh_num.cc',
                                         5, rank=4,
                                         diagname='TTbarHDiagram')
    mynum.writeSource()

if __name__ == "__main__":
    main()

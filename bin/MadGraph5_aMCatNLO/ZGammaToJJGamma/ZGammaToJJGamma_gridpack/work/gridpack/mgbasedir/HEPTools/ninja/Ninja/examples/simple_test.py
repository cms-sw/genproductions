#! /usr/bin/env python

import ninjanumgen

def main():
    # define the mandatory arguments for the constructor
    n_legs = 4
    input_file = 'mynum.frm'
    output_file = 'mynum.cc'
    
    # define an instance of the class DiagramExpansion
    mynum = ninjanumgen.DiagramExpansion(input_file,
                                         output_file,
                                         n_legs,rank=4)
    
    # generate the source
    mynum.writeSource()

if __name__ == "__main__":
    main()

#! /usr/bin/env python

import ninjanumgen

def main():
    # define the mandatory arguments for the constructor
    n_legs = 5
    input_file = 'mynumhr.frm'
    output_file = 'mynumhr.cc'
    
    # define an instance of the class DiagramExpansion
    mynum = ninjanumgen.DiagramExpansion(input_file,
                                         output_file,
                                         n_legs,rank=6)
    
    # generate the source
    mynum.writeSource()

if __name__ == "__main__":
    main()

The goal of the parsed code is the syntactic analysis of the Madgraph cards to check:

-The cards's right structure.

-Wrong objects definition.

-Possible bugs.

The code is written in python and it take as input:

-Proc card

-Run card

Proc card checks:

-Only one proton definition is permitted

-If the proton is defined with “b” quarks, the parser check if the correct pdf-set is used in the run card. To to this the pdf's number of flavour is taken form https://lhapdf.hepforge.org/pdfsets.html

-The add-process line must contain generate line. The possible jet must be defined.

-The model line must be defined.

-The card must include output line with correct.

Inside the Run card:

-13 TeV energy of collision.

-Declaration of nevents

-If ickkw=1 checks if the jets are in the process
-No double declaration of pfd number.


How to run the code:

********

python parsing.py name-of-cards e.g. python parsing.py dyellell012j-5f-NLO-FXFX 

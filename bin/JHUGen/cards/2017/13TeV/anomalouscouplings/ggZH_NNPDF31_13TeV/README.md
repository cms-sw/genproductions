Note about the anomalous coupling cards here:

ggZH comes from two diagrams: the triangle with HZZ couplings

```
~~~~|\
    | >~~~~~~~~~~~~~~~~
~~~~|/      \ _ _ _ _ _ 
```
             

and the box with Hff couplings

```
     _____
~~~~|     |~~~~~~~~
    |     |
~~~~|_____| _ _ _ _
```


In this folder we provide the same input cards as in the other folders: SM, and values of 1 and 0.5 for
4 anomalous couplings: fa3, fa2, fL1, and fL1Zg.

These anomalous couplings only affect the triangle diagram.
The box is always fixed to its SM, scalar coupling.
In addition, the triangle amplitude evaluates to 0 for a3.
That means that fa3=1 is a pure box, and fa3=0.5 is a mixture of the box and triangle different from the SM.

For a complete analysis of ggZH anomalous couplings, you'll have to use reweighting through MELA.
Please contact the JHUGen authors for more information.

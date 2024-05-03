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

Of the anomalous couplings we provide for the other production mechanisms,
only L1 and L1Zg contribute to the triangle.  The a3 and a2 terms integrate to 0
over the loop.

In the L1 and L1Zg (and L1mix and L1Zgmix) cards in this folder,
the box is fixed to its SM, scalar coupling.

In addition, we provide box-only and triangle-only cards here.
In a search for a3 or a2 anomalous couplings, the triangle tensor
structure is not affected, but its contribution and interference with the box
shrinks.  You'll want to use the triangle-only, box-only, and SM cards to
extract the triangle, box, and interference contributions.

In the case of L1 and L1Zg, their interference with the box is not
provided here.  For a complete analysis of ggZH anomalous couplings,
you'll have to use reweighting through MELA.
Please contact the JHUGen authors for more information.

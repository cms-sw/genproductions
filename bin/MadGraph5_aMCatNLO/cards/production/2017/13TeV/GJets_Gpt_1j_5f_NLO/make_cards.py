#!/bin/env python3
import shutil
import os
pjoin = os.path.join
reference = "GJets_1j_Gpt-100To250_5f_NLO"


bin_bounds = [
    (250,400),
    (400,650),
    (650,"Inf")
]


for ptmin, ptmax in bin_bounds:
    name=f"GJets_1j_Gpt-{ptmin}To{ptmax}_5f_NLO"
    shutil.copytree(reference,name)

    
    for basename in os.listdir(name):
        # Rename files
        old_path = pjoin(name, basename)
        new_path = pjoin(name, basename.replace(reference, name))
        shutil.move(old_path, new_path)

        # Replace the name if it occurs in the file
        with open(new_path,"r") as f:
            text = f.read()
        with open(new_path,"w") as f:
            text = text.replace(reference, name)
            f.write(text)
        
        # Replace the value in cuts.f
        if "cuts" in basename:
            # Delete cuts.f for highest bin
            if ptmax=="Inf":
                os.remove(new_path)
                continue

            # Fill in the values for the other bins
            with open(new_path,"r") as f:
                text = f.read()
            with open(new_path,"w") as f:
                text = text.replace("if(ptg.gt.250)then", f"if(ptg.gt.{ptmax})then")
                f.write(text)

        # Replace the value in the run card
        if "run" in basename:
            with open(new_path,"r") as f:
                text = f.read()
            with open(new_path,"w") as f:
                text = text.replace("100  = ptgmin", f"{ptmin}  = ptgmin")
                f.write(text)

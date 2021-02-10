# H->ZA->llbb gridpacks production:
##Default: Preparing Cards 
```bash
Run as follow:
./prepare_MG5_cards.py --order --test
```
```
--order : LO or NLO computation 
    LO   ggfusion - loop induced
    NLO  b-associated production 
--test : will produce 1 set of cards for ecah process, saved by defauly in example_cards/ and 2 bash scripts prepare_example_{order}_gridpacks.sh to genrate the gridpacks
    LO   tb= 1.5 , MH= 500, MA=300 ,
    NLO  tb= 20. , MH=500, MA=300
-- lhaid : will be set to $DEFAULT_PDF_SETS as shortcuts to have the PDF sets automatically and added to the run_card at run time to avoid specifying them directly
    lhapdf = pdlabel ! PDF set
    $DEFAULT_PDF_SETS = lhaid
    $DEFAULT_PDF_MEMBERS  = reweight_PDF
--queue : 1nh
```
##Genrate gridpacks:
```bash
./prepare_example_nlo_gridpacks.sh
./prepare_example_lo_gridpacks.sh
```

import argparse

parser = argparse.ArgumentParser()
parser.add_argument("outdir", help="directory name to output files to", default = './', type=str)
args = parser.parse_args()

# VARIABLES:
gmasses = range(100, 2100, 100)
# add M = 150 Gev for low mass analysis
gmasses.append(150)
hmass_ratio = 0.6
# :VARIABLES



# FUNCTIONS:
def make_new_script(f_in, args, f_out):
	with open(f_in) as fin:
		text = fin.read()
	for key, value in args.items():
		text = text.replace("%%{0}%%".format(key.upper()), str(value))
	with open(f_out, "w") as fout:
		fout.write(text)

def main():
	for gmass in gmasses:
		name = "RPV_GluinoGluinoToJets_M-{0}_customizecards.dat".format(gmass)
		make_new_script(
			"RPV_GluinoGluinoToJets_customizecards.template",
			{
				"gmass": gmass
			},
			args.outdir + name
		)
		
		name = "RPV_GluinoGluinoToJets_M-{0}_proc_card.dat".format(gmass)
		make_new_script(
			"RPV_GluinoGluinoToJets_proc_card.template",
			{
				"gmass": gmass
			},
			args.outdir + name
		)
                name = "RPV_GluinoGluinoToJets_M-{0}_run_card.dat".format(gmass)
                make_new_script(
                        "RPV_GluinoGluinoToJets_run_card.template",
                        {
                                "gmass": gmass
                        },
                        args.outdir + name
                )
                name = "RPV_GluinoGluinoToJets_M-{0}_extramodels.dat".format(gmass)
                make_new_script(
                        "RPV_GluinoGluinoToJets_extramodels.template",
                        {
                                "gmass": gmass
                        },
                        args.outdir + name
                )

		
	return True
# :FUNCTIONS

# MAIN:
if __name__ == "__main__":
	main()
# :MAIN

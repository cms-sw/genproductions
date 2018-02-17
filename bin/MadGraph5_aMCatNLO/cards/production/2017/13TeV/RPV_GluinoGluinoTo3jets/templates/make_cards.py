####################################################################
# Type: SCRIPT                                                     #
#                                                                  #
# Description: [description]                                       #
####################################################################

# IMPORTS:
# :IMPORTS

# CLASSES:
# :CLASSES

# VARIABLES:
gmasses = range(200, 2100, 100)
hmass_ratio = 0.6
# :VARIABLES

# FUNCTIONS:
def make_new_script(f_in, args, f_out):
	with open(f_in) as fin:
		text = fin.read()
	for key, value in args.items():
		text = text.replace("%%{}%%".format(key.upper()), str(value))
	with open(f_out, "w") as fout:
		fout.write(text)

def main():
	for gmass in gmasses:
		name = "RPV_GluinoGluinoToJets_customizecards_M-{}.dat".format(gmass)
		make_new_script(
			"RPV_GluinoGluinoToJets_customizecards.template",
			{
				"gmass": gmass
			},
			name
		)
		
		name = "RPV_GluinoGluinoToJets_proc_card_M-{}.dat".format(gmass)
		make_new_script(
			"RPV_GluinoGluinoToJets_proc_card.template",
			{
				"gmass": gmass
			},
			name
		)
		
	return True
# :FUNCTIONS

# MAIN:
if __name__ == "__main__":
	main()
# :MAIN

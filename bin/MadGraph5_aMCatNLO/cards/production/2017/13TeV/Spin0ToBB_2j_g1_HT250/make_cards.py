import os
import sys

card_types = ["customizecards", "extramodels", "proc_card", "run_card"]
masses = [50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500]
for mass in masses:
	output_card_dir = "cards/scalar_m{}".format(mass)
	os.system("mkdir -pv {}".format(output_card_dir))
	for card_type in card_types:
		with open("template_cards/Spin0ToBB_2j_scalar_g1_HT250_MX_{}.dat".format(card_type), 'r') as input_card:
			with open("{}/Spin0ToBB_2j_scalar_g1_HT250_M{}_{}.dat".format(output_card_dir, mass, card_type), 'w') as output_card:
				for line in input_card:
					output_card.write(line.replace("@MASS@", str(mass)))
				output_card.write("\n")

	output_card_dir = "cards/pseudoscalar_m{}".format(mass)
	os.system("mkdir -pv {}".format(output_card_dir))
	for card_type in card_types:
		with open("template_cards/Spin0ToBB_2j_pseudoscalar_g1_HT250_MX_{}.dat".format(card_type), 'r') as input_card:
			with open("{}/Spin0ToBB_2j_pseudoscalar_g1_HT250_M{}_{}.dat".format(output_card_dir, mass, card_type), 'w') as output_card:
				for line in input_card:
					output_card.write(line.replace("@MASS@", str(mass)))
				output_card.write("\n")

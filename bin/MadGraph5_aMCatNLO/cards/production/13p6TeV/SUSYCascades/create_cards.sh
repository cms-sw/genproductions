#!/bin/bash
for massPoint in 260 270
do
	destinationPath=SlepSnuCascade_MNeu"$massPoint"
	if [ -d "$destinationPath" ]; then #check if directory already exists
		echo "The directory $destinationPath already exists. It will be deleted before creating a new one."
		rm -rf $destinationPath
	fi

	mkdir $destinationPath

	#process cards
	cp SlepSnuCascade_MNeu220/SlepSnuCascade_MNeu220_TuneCP5_13p6TeV-madgraphMLM_proc_card.dat SlepSnuCascade_MNeu"$massPoint"/SlepSnuCascade_MNeu"$massPoint"_TuneCP5_13p6TeV-madgraphMLM_proc_card.dat
	newProcCard=SlepSnuCascade_MNeu"$massPoint"/SlepSnuCascade_MNeu"$massPoint"_TuneCP5_13p6TeV-madgraphMLM_proc_card.dat
	sed -i -e "s/output SlepSnuCascade_MNeu220_TuneCP5_13p6TeV-madgraphMLM/output SlepSnuCascade_MNeu${massPoint}_TuneCP5_13p6TeV-madgraphMLM/" ${newProcCard}

	#param cards
	cp SlepSnuCascade_MNeu220/SlepSnuCascade_MNeu220_TuneCP5_13p6TeV-madgraphMLM_param_card.dat SlepSnuCascade_MNeu"$massPoint"/SlepSnuCascade_MNeu"$massPoint"_TuneCP5_13p6TeV-madgraphMLM_param_card.dat
	newParamCard=SlepSnuCascade_MNeu"$massPoint"/SlepSnuCascade_MNeu"$massPoint"_TuneCP5_13p6TeV-madgraphMLM_param_card.dat

	if [ "$massPoint" -eq 260 ]; then
		sed -i -e "s/1000022 2.200000e+02 # Mneu1/1000022 2.600000e+02 # Mneu1/" ${newParamCard}
		sed -i -e "s/1000023 2.600000e+02 # Mneu2/1000023 2.800000e+02 # Mneu2/" ${newParamCard}
		sed -i -e "s/1000024 2.400000e+02 # Mch1/1000024 2.700000e+02 # Mch1/" ${newParamCard}
	else
		sed -i -e "s/1000022 2.200000e+02 # Mneu1/1000022 2.700000e+02 # Mneu1/" ${newParamCard}
		sed -i -e "s/1000023 2.600000e+02 # Mneu2/1000023 2.800000e+02 # Mneu2/" ${newParamCard}
		sed -i -e "s/1000024 2.400000e+02 # Mch1/1000024 2.750000e+02 # Mch1/" ${newParamCard}
	fi

	#run cards
	cp SlepSnuCascade_MNeu220/SlepSnuCascade_MNeu220_TuneCP5_13p6TeV-madgraphMLM_run_card.dat SlepSnuCascade_MNeu"$massPoint"/SlepSnuCascade_MNeu"$massPoint"_TuneCP5_13p6TeV-madgraphMLM_run_card.dat
done

for i in `seq $1 $2`; do
name=${3}

	file2=7TeV_${name}_run${i}_unweighted_events

			#echo Copying file 7TeV_${name}_run${i}_unweighted_events....
			if [[ -f /pnfs/iihe/cms/store/user/alkaloge/7TeV_Summer11/${name}/7TeV_${name}_run${i}_unweighted_events.lhe.gz || -f 7TeV_${name}_run${i}_unweighted_events.lhe ]]; then
				        dccp /pnfs/iihe/cms/store/user/alkaloge/7TeV_Summer11/${name}/7TeV_${name}_run${i}_unweighted_events.lhe.gz .
					gzip -d 7TeV_${name}_run${i}_unweighted_events.lhe.gz


			fi
		done


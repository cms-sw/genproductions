# set -e

GREEN=$'\e[0;32m'
RED=$'\e[0;31m'
NC=$'\e[0m'

for dir in cards/production/2017/13TeV/NMSSM_XYH_YToHH_6b/NM*
do
   path=${dir%*/}
   dir="${path##*/}"
   echo ".. generating gridpack for ${dir}"
    ./gridpack_generation.sh ${dir} ${path}
    echo ""
done
cd ../..
python3 bin/utils/request_fragment_check.py --bypass_status --develop --download_json --prepid $1
python3 bin/utils/request_fragment_check.py --bypass_status --develop --local --prepid $1

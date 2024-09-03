python3.9 -m venv venv && source ./venv/bin/activate
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
echo "If necessary, run:"
echo "pip install git+https://github.com/cms-PdmV/mcm_scripts.git"
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
echo
#pip install git+https://github.com/cms-PdmV/mcm_scripts.git
python3 request_fragment_check.py --bypass_status --develop --prepid $1

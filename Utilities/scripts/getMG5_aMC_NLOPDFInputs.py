from python.PDFSetsChooserTools import PDFSetHelper_MG5_aMC_NLO
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-f', '--format', choices=['members', 'sets'], required=True,
        help='Output PDF set list or list of members to store')
parser.add_argument('-c', '--pdf_choice', choices=['custom', '2016', '2017'],
        required=True, help="Use 2017 or 2016 defaults or custom choice")
parser.add_argument('--is5FlavorScheme', action='store_true',
        help='Use PDF set for 5 flavor scheme (vs. 4F if false)')

args = parser.parse_args()

helper = PDFSetHelper_MG5_aMC_NLO()
if args.pdf_choice:
    helper.readDefaultPDFsFile(args.is5FlavorScheme)
else:
    #TODO Implement option for custom PDF list
    print "Custom sets not yet supported"
    exit(1)

if args.format == "sets":
    print helper.getListOfLHAPDFIds()
    exit(0)
elif args.format == "members":
    print helper.getListOfMembersToStore()
    exit(0)

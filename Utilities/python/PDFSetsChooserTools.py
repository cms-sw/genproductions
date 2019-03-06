import os

class PDFSetHelper(object):
    def __init__(self, pdflist_file=None):
        self.pdflist_file = pdflist_file
        self.pdf_data = []
        if self.pdflist_file:
            self.readPDFsFromFile()

    def readDefaultPDFsFile(self, is5FlavorScheme):
        base_dir = os.path.dirname(os.path.realpath(__file__)) 
        meta_data_dir = base_dir.replace("Utilities/python", "MetaData") 

        self.pdflist_file = meta_data_dir + "/" + ("pdflist_5f_2017.dat" \
            if is5FlavorScheme else "pdflist_4f_2017.dat")
        self.readPDFsFromFile()

    def readPDFsFromFile(self):
        with open(self.pdflist_file) as pdflist_file:
            for line in pdflist_file.readlines():
                split_line = line.split("#")[0]
                line_info = split_line.split()
                if len(line_info) == 3:
                    line_info[2] = int(line_info[2])
                    self.pdf_data.append(line_info)

    def getPDFData(self):
        return self.pdf_data
        
class PDFSetHelper_MG5_aMC(PDFSetHelper):
    def __init__(self, pdflist_file=None):
        super(PDFSetHelper_MG5_aMC, self).__init__(pdflist_file)

    def getListOfLHAPDFIds(self, isNLO):
        if isNLO:
            return ','.join([x[0] for x in self.pdf_data])
        return self.pdf_data[0][0]

    def getListOfMembersToStore(self):
        return ','.join(['True' if x[2] > 1 else 'False' for x in self.pdf_data])

    # Format pdf list for systematics program
    # See https://cp3.irmp.ucl.ac.be/projects/madgraph/wiki/Systematics
    def getListOfLHAPDFIdsForSystematics(self):
        sys_list = [ x[0] if x[2] > 1 else str(x[0])+"@0" for x in self.pdf_data]
        return ','.join(sys_list)

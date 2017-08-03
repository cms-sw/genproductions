// g++ -Wall -o mergeLheFiles mergeLheFiles.cpp

#include <iostream>
#include <fstream>
#include <vector>

int main(int argc, char** argv)
{
  if(argc != 3 ) {
    std::cout << ">>>mergeLheFile.cpp::Usage:   " << argv[0] << "   normalfile.lhe   pdfWeights.lhe" << std::endl;
    return -1;
  }
  
  char* initialFileName = argv[1]; 
  std::cout << "normalFileName = " << initialFileName << std::endl;
  char* pdfFileName = argv[2]; 
  std::cout << "pdfFileName = " << pdfFileName << std::endl;  
  
  // open lhe files
  std::ifstream initialFile(initialFileName, std::ios::in);
  std::ifstream pdfFile(pdfFileName, std::ios::in);
  std::ofstream outFile("out.lhe", std::ios::out);

  std::string line;
  std::string line2;
  bool getFromPdf = false;
  bool getFromNormal = true;
  int eventIt = 0;

  while(!initialFile.eof() && !pdfFile.eof()) {
    
    if (getFromPdf) getline(pdfFile, line);
    if (getFromNormal) getline(initialFile, line);
    if (!initialFile.good() || !pdfFile.good()) break;   
    
    outFile << line << std::endl;

    if (line.find("nnlops-0505-9") != std::string::npos) {
      getFromNormal = false;  getFromPdf = true;
      if (!eventIt) {
	getline(initialFile, line2);
        outFile << "</weightgroup>" << std::endl;
      } 
      eventIt++;
    }	

    if (line.find("</rwgt>") != std::string::npos || line.find("</initrwgt>") != std::string::npos) {
      getFromNormal = true;  getFromPdf = false;
      getline(initialFile, line2);
      if (eventIt>1) {
	for (int i = 0; i< 6; i++) {getline(pdfFile, line2);}
      } 
    }	
  }
  outFile << "</LesHouchesEvents>" << std::endl;

  std::cout << "Merged " << eventIt-1 << " events " << std::endl;
  return 0;
}



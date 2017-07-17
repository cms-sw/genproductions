// g++ -Wall -o splitLheFiles splitLheFiles.cpp

#include <iostream>
#include <fstream>
#include <vector>

int main(int argc, char** argv)
{
  if(argc < 3) {
    std::cout << ">>>splitLheFile.cpp::Usage:   " << argv[0] << "   initialFile.lhe   fileToAdd1.lhe   fileToAdd2.lhe ..." << std::endl;
    return -1;
  }
  
  char* initialFileName = argv[1]; 
  std::cout << "initialFileName = " << initialFileName << std::endl;
  
  std::vector<char*> fileToAddNames;
  for(int fileIt = 0; fileIt < argc-2; ++fileIt) {
    fileToAddNames.push_back( argv[2+fileIt] );
    std::cout << "fileToAddName = " << fileToAddNames.at(fileIt) << std::endl;
  }
  
  // open lhe files
  std::ifstream initialFile(initialFileName, std::ios::in);
  std::ofstream outFile("out.lhe", std::ios::out);
  std::ofstream pdfFile("pdf.lhe", std::ios::out);
   
  std::string line;
  std::string line2;
  bool writeEvent = true;
  bool writePdf = false;
  int eventIt = 0;
  std::string pdfString = "wgt id=\'2001\'";

  while(!initialFile.eof()) {
    
    getline(initialFile, line);
    if( !initialFile.good() ) break;   
    
    if( line != "</LesHouchesEvents>" ) { 
      
      if( line == "<event>" || line == "</initrwgt>" ) {
	++eventIt;
	writeEvent = true;   writePdf = false;
      }
      
      if( line.find(pdfString) != std::string::npos || line.find("PDF_variation") != std::string::npos) {
	writeEvent = false;   writePdf = true;
      }
      
      if( line == "</rwgt>" ) {
	writeEvent = true;   writePdf = true;
      }
      
      if (writeEvent) outFile << line << std::endl;
      if (writePdf) pdfFile << line << std::endl;     
    
    } else {
      for(int fileIt = 0; fileIt < argc-2; ++fileIt) {
        std::ifstream fileToAdd(fileToAddNames.at(fileIt), std::ios::in);
        
        writeEvent = false;
        writePdf = false;

        while(!fileToAdd.eof()) {
          getline(fileToAdd, line2); 
          if( !fileToAdd.good() || line2 == "</LesHouchesEvents>" ) break;
          
          if( line2 == "<event>" ) {
	    ++eventIt;
	    writeEvent = true;   writePdf = false;
	  }
	  
	  if( line2.find(pdfString) != std::string::npos ) {
	    writeEvent = false;   writePdf = true;
	  }

	  if( line2 == "</rwgt>" ) {
	    writeEvent = true;   writePdf = true;
	  }
	  
	  if (writeEvent) outFile << line2 << std::endl;
	  if (writePdf) pdfFile << line2 << std::endl;
          
        }
      }
    }
  }
  outFile << "</LesHouchesEvents>" << std::endl;
  
  std::cout << "Added " << eventIt << " events from " << argc-2 << " files to file " << initialFileName << std::endl;
  return 0;
}



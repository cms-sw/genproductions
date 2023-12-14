#include <iostream>
#include <fstream>
#include <deque>
#include "Math/Vector4D.h"
#include "Math/GenVector/Boost.h"
#include "TFile.h"
#include "TTreeReader.h"
#include "TTreeReaderValue.h"


void convert_UGROOT2LHE(const std::string& inFileName, const double& beamE1, const double& beamE2)
{
  // Open input file
  TFile inFile(inFileName.c_str(), "READ");
  if (not inFile.IsOpen())
    throw std::logic_error("[convert_UGROOT2LHE] Failed to open input file: "+inFileName);

  // Create output file
  const auto outFileName = inFileName.substr(0, inFileName.rfind(".")).substr(inFileName.rfind("/")+1) + ".lhe";
  std::ofstream outFile(outFileName);
  if (not outFile.is_open())
    throw std::logic_error("[convert_UGROOT2LHE] Failed to open output file: "+outFileName);

  std::cout << "Converting UPCGen ROOT output to LHE format" << std::endl;

  // Add header
  outFile << "<LesHouchesEvents version=\"3.0\">" << std::endl;
  outFile << "<!-- " << std::endl << " #Converted from UPCGEN generator HEPMC output " << std::endl << "-->" << std::endl;
  outFile << "<header>" << std::endl << "</header>" << std::endl;

  // Put generic initialization-level info since UPCGen doesn't save this
  outFile << "<init>" << std::endl;
  //beam particle 1, 2, beam energy 1, 2, author group, beam 1, 2, PDFSET beam 1, 2,
  outFile << "22 " << "22 " << beamE1 << " " << beamE2 << " 0 " << "0 " << "0 " << "0 " << "3 " << "1" << std::endl;
  outFile << "1.0 " << "0.0 " << "3.0 " << "81" << std::endl;
  outFile << "</init>" << std::endl;

  int nEvt(0), iEvt(-1), iEntry(-1);
  ROOT::Math::PxPyPzEVector momP(0,0,0,0);
  std::string line, label, version;
  std::deque<std::string> oLines;
  outFile.precision(10);

  // Initialise input tree reader
  TTreeReader myReader("particles", &inFile);
  TTreeReaderValue<int>    eventNumber(myReader, "eventNumber");
  TTreeReaderValue<int>    pdgCode(myReader, "pdgCode");
  TTreeReaderValue<int>    particleID(myReader, "particleID");
  TTreeReaderValue<int>    motherID(myReader, "motherID");
  TTreeReaderValue<double> px(myReader, "px");
  TTreeReaderValue<double> py(myReader, "py");
  TTreeReaderValue<double> pz(myReader, "pz");
  TTreeReaderValue<double> en(myReader, "e");

  // Read input file
  const auto& nEntries = myReader.GetEntries();
  while (iEntry<nEntries-1) {
    // Read new event entry
    iEntry = myReader.GetCurrentEntry();
    if (iEntry>=0 && iEvt!=*eventNumber) {
      iEvt = *eventNumber;
      momP = ROOT::Math::PxPyPzEVector(0,0,0,0);
      oLines.clear();
    }

    // Read particle entry
    if (iEntry>=0) {
      std::ostringstream oLine;
      const auto& momI = *motherID;
      const auto& parI = *particleID;
      ROOT::Math::PxPyPzEVector p(*px, *py, *pz, *en);
      if (parI != oLines.size() || momI >= parI)
        throw std::logic_error("[convert_UGROOT2LHE] Failed to extract particle entry!");
      oLine << *pdgCode << " 1 " << (momI>0 ? momI+2 : 1) << " " << momI+2 << " 0 0 " << p.Px() << " " << p.Py() << " " << p.Pz() << " " << p.E() << " " << p.M() << " 0.0 9.0" << std::endl;
      oLines.push_back(oLine.str());
      if (momI > 0)
        oLines[momI-1].replace(oLines[momI-1].find(" ")+1, 1, "2");
      else
        momP += p;
    }

    // Read next event
    myReader.Next();

    // Finalise previous event
    if (iEntry==nEntries-1 || (iEntry>0 && iEvt!=*eventNumber)) {
      // Add fake photons
      ROOT::Math::Boost boost(momP.BoostToCM());
      const auto momCM = boost(momP);
      const auto phoCM = ROOT::Math::PxPyPzEVector(0, 0, momCM.E()/2, momCM.E()/2);
      boost.Invert();
      for (const auto& p : {boost(phoCM), boost(momCM - phoCM)}) {
        std::ostringstream oLine;
        oLine << "22 3 0 0 0 0 " << p.Px() << " " << p.Py() << " " <<  p.Pz() << " " << p.E() << " 0.0 0.0 9.0" << std::endl;
        oLines.push_front(oLine.str());
      }

      // Write LHE file
      outFile << "<event>" << std::endl;
      outFile << oLines.size() << " 81" << " 1.0 -1.0 -1.0 -1.0" << std::endl;
      for (const auto& oLine : oLines)
        outFile << oLine;
      outFile << "</event>" << std::endl;
      nEvt++;
    }
  }

  outFile << "</LesHouchesEvents>" << std::endl;
  outFile.close();
  inFile.Close();
  std::cout << nEvt << " events written in " << outFileName << std::endl;
};


int main(int argc, char *argv[])
{
  if (argc != 4) {
    std::cout << "Invalid input parameters!" << std::endl;
    std::cout << "Usage: ./convert_UGROOT2LHE <INPUT_FILE> <BEAM_1_E> <BEAM_2_E>" << std::endl;
    return 1;
  }
  convert_UGROOT2LHE(std::string(argv[1]), std::atof(argv[2]), std::atof(argv[3]));
  return 0;
};

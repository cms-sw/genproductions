#include <iostream>
#include <fstream>
#include <vector>
#include "Math/Vector4D.h"
#include "Math/GenVector/Boost.h"


void convert_UGHEPMC2LHE(const std::string& inFileName, const double& beamE1, const double& beamE2)
{
  // Open input file
  std::ifstream inFile(inFileName);
  if (not inFile.is_open())
    throw std::logic_error("[convert_UGHEPMC2LHE] Failed to open input file: "+inFileName);

  // Create output file
  const auto outFileName = inFileName.substr(0, inFileName.rfind(".")).substr(inFileName.rfind("/")+1) + ".lhe";
  std::ofstream outFile(outFileName);
  if (not outFile.is_open())
    throw std::logic_error("[convert_UGHEPMC2LHE] Failed to open output file: "+outFileName);

  std::cout << "Converting UPCGen HEPMC output to LHE format" << std::endl;

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

  int nEvt(0);
  std::string line, label, version;
  outFile.precision(10);

  // Read input file
  while (getline(inFile, line) &&
         line.rfind("END_EVENT_LISTING")==std::string::npos) {
    // Read event line
    if (line.rfind("E ", 0)!=0) continue;

    // E n nvertices nparticles
    int iEvt, nVtx, nPar;
	if (not (std::istringstream(line) >> label >> iEvt >> nVtx >> nPar) || label != "E")
      throw std::logic_error("[convert_UGHEPMC2LHE] Failed to parse event line: "+line);

    // U momentum_unit length_unit
    if (not getline(inFile, line) || not (std::istringstream(line) >> label) || label != "U")
      throw std::logic_error("[convert_UGHEPMC2LHE] Failed to parse event line: "+line);

    // Read particle lines
    ROOT::Math::PxPyPzEVector momP(0,0,0,0);
    std::vector<std::string> oLines(nPar+2);
    for (size_t iPar=0; iPar < nPar; iPar++) {
      // P n nv pdgid px py pz e mass status
      double px, py, pz, en, mass;
      int parI, momI, pdgId, status;
      if (not getline(inFile, line) || not (std::istringstream(line) >> label >> parI >> momI >> pdgId >> px >> py >> pz >> en >> mass >> status) || label != "P" || parI != iPar+1 || momI >= parI)
        throw std::logic_error("[convert_UGHEPMC2LHE] Failed to parse track line: "+line);
      std::ostringstream oLine;
      oLine << pdgId << " 1 " << (momI>0 ? momI+2 : 1) << " " << momI+2 << " 0 0 " << px << " " << py << " " << pz << " " << en << " " << mass << " 0.0 9.0" << std::endl;
      oLines[iPar+2] = oLine.str();
      if (momI > 0)
        oLines[momI-1].replace(oLines[momI-1].find(" ")+1, 1, "2");
      else
        momP += ROOT::Math::PxPyPzEVector(px, py, pz, en);
    }

    // Add fake photons
    ROOT::Math::Boost boost(momP.BoostToCM());
    const auto momCM = boost(momP);
    const auto phoCM = ROOT::Math::PxPyPzMVector(0, 0, momCM.E()/2, 0);
    boost.Invert();
    const auto p1 = boost(phoCM);
    const auto p2 = boost(momCM - phoCM);
    std::ostringstream fp1, fp2;
    fp1 << "22 3 0 0 0 0 " << p1.Px() << " " << p1.Py() << " " <<  p1.Pz() << " " << p1.E() << " 0.0 0.0 9.0" << std::endl;
    fp2 << "22 3 0 0 0 0 " << p2.Px() << " " << p2.Py() << " " <<  p2.Pz() << " " << p2.E() << " 0.0 0.0 9.0" << std::endl;
    oLines[0] = fp1.str();
    oLines[1] = fp2.str();

    // Write LHE file
    outFile << "<event>" << std::endl;
    outFile << oLines.size() << " 81" << " 1.0 -1.0 -1.0 -1.0" << std::endl;
    for (const auto& oLine : oLines)
        outFile << oLine;
    outFile << "</event>" << std::endl;
    nEvt++;
  }

  outFile << "</LesHouchesEvents>" << std::endl;
  outFile.close();
  inFile.close();
  std::cout << nEvt << " events written in " << outFileName << std::endl;
};


int main(int argc, char *argv[])
{
  if (argc != 4) {
    std::cout << "Invalid input parameters!" << std::endl;
    std::cout << "Usage: ./convert_UGHEPMC2LHE <INPUT_FILE> <BEAM_1_E> <BEAM_2_E>" << std::endl;
    return 1;
  }
  convert_UGHEPMC2LHE(std::string(argv[1]), std::atof(argv[2]), std::atof(argv[3]));
  return 0;
};

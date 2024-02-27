#include <iostream>
#include <fstream>
#include <vector>
#include "Math/Vector4D.h"
#include "TFile.h"
#include "TTreeReader.h"
#include "TTreeReaderValue.h"


void convert_UGROOT2LHE(const std::string& inFileName, const double& beamE1, const double& beamE2)
{
  // Open input file
  TFile inFile(inFileName.c_str(), "READ");
  if (not inFile.IsOpen())
    throw std::logic_error("[convert_UGROOT2LHE] Failed to open input file: "+inFileName);

  // Extract cross section
  double fidxsec(1), totxsec(3);
  std::ifstream xsecFile(inFileName.substr(0, inFileName.rfind("/")+1)+"xsec.out");
  if (xsecFile.is_open())
    xsecFile >> fidxsec >> totxsec;

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
  outFile << std::fixed << std::setprecision(8) << std::scientific;
  //LHE format: https://arxiv.org/pdf/hep-ph/0109068.pdf
  //beam pdg id (1, 2), beam energy [GeV] (1, 2), PDF author group (1, 2), PDF set id (1, 2), weight strategy, # subprocesses
  outFile << "2212 2212 " << beamE1 << " " << beamE2 << " 0 0 0 0 3 1" << std::endl;
  //cross section [pb], cross section stat. unc. [pb], maximum event weight, subprocess id
  outFile << fidxsec << " " << 0.0 << " " << totxsec << " 81" << std::endl;
  outFile << "</init>" << std::endl;

  int nEvt(0), iEvt(-1), iEntry(-1);
  // Particle tuple: pdg id, status, mother index, 4-momentum
  std::vector<std::tuple<int, int, int, ROOT::Math::PxPyPzEVector>> parV;
  parV.reserve(10);

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
      parV.clear();
    }

    // Read particle entry
    if (iEntry>=0) {
      const auto& momI = *motherID;
      const auto& parI = *particleID;
      if (parI != parV.size() || momI >= parI)
        throw std::logic_error("[convert_UGROOT2LHE] Failed to extract particle entry!");

      parV.emplace_back(*pdgCode, 1, momI, ROOT::Math::PxPyPzEVector(*px, *py, *pz, *en));
      if (momI > 0)
        std::get<1>(parV[momI-1]) = 2;
    }

    // Read next event
    myReader.Next();

    // Finalise previous event
    if (iEntry==nEntries-1 || (iEntry>0 && iEvt!=*eventNumber)) {
      // Write event
      outFile << "<event>" << std::endl;
      //# particles, subprocess id, event weight, event scale, alpha_em, alpha_s
      outFile << parV.size() << " 81 1.0 -1.0 -1.0 -1.0" << std::endl;
      outFile << std::fixed << std::setprecision(10) << std::scientific;
      for (const auto& pV : parV) {
        const auto& [pdgId, status, momI, p] = pV;
        //particle: pdg id, status, mother index (1, 2), color flow tag (1, 2), (px, py, pz, energy, mass [GeV]), proper lifetime [mm], spin
        outFile << pdgId << " " << status << " " << momI << " 0 0 0 " << p.Px() << " " << p.Py() << " " << p.Pz() << " " << p.E() << " " << p.M() << " 0.0000e+00 9.0000e+00" << std::endl;
      }
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

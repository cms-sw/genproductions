#include <iostream>
#include <fstream>
#include <vector>
#include "Math/Vector4D.h"


void convert_SCLHE2LHE(const std::string& inFileName)
{
  // Open input file
  std::ifstream inFile(inFileName);
  if (not inFile.is_open())
    throw std::logic_error("[convert_SCLHE2LHE] Failed to open input file: "+inFileName);

  // Create output file
  const auto outFileName = inFileName.substr(0, inFileName.rfind(".")).substr(inFileName.rfind("/")+1) + "_proc.lhe";
  std::ofstream outFile(outFileName);
  if (not outFile.is_open())
    throw std::logic_error("[convert_SCLHE2LHE] Failed to open output file: "+outFileName);

  std::cout << "Converting SuperChic LHE output to LHE format" << std::endl;

  // Add header
  outFile << "<LesHouchesEvents version=\"3.0\">" << std::endl;
  outFile << "<!-- " << std::endl << " #Converted from SUPERCHIC generator LHE output " << std::endl << "-->" << std::endl;
  outFile << "<header>" << std::endl << "</header>" << std::endl;

  int nEvt(0);
  std::string line;

  // Read input file
  while (getline(inFile, line)) {
    // Read init
    if (line.rfind("<init>")!=std::string::npos) {
      int beamId1, beamId2, pdfa1, pdfa2, pdfs1, pdfs2, wAlgo, nProc, d;
      double beamE1, beamE2, xsec, xsecErr, wMax;
      if (not getline(inFile, line) || not (std::istringstream(line) >> beamId1 >> beamId2 >> beamE1 >> beamE2 >> pdfa1 >> pdfa2 >> pdfs1 >> pdfs2 >> wAlgo >> nProc) || nProc != 1)
        throw std::logic_error("[convert_SCLHE2LHE] Failed to parse init line: "+line);
      // PYTHIA fix for low sigND warning (beam energy: total --> per nucleon)
      const auto A1 = beamId1>1E9 ? ((beamId1 / 10) % 1000) : 1;
      const auto A2 = beamId2>1E9 ? ((beamId2 / 10) % 1000) : 1;
      beamE1 /= A1;
      beamE2 /= A2;
      // Ion beams only supported in PYTHIA version >= 8.308
      // PYTHIA v8.306 fix (beam pdgId: ion --> proton)
      beamId1 = beamId1>1E9 ? 2212 : beamId1;
      beamId2 = beamId2>1E9 ? 2212 : beamId2;
      // Put initialization-level info
      outFile << "<init>" << std::endl;
      outFile << std::fixed << std::setprecision(8) << std::scientific;
      //LHE format: https://arxiv.org/pdf/hep-ph/0109068.pdf
      //beam pdg id (1, 2), beam energy [GeV] (1, 2), PDF author group (1, 2), PDF set id (1, 2), weight strategy, # subprocesses
      outFile << beamId1 << " " << beamId2 << " " << beamE1 << " " << beamE2 << " " << pdfa1 << " " << pdfa2 << " " << pdfs1 << " " << pdfs2 << " " << wAlgo << " " << nProc << std::endl;
      if (not getline(inFile, line) || not (std::istringstream(line) >> xsec >> xsecErr >> wMax >> d))
        throw std::logic_error("[convert_SCLHE2LHE] Failed to parse init line: "+line);
      //cross section [pb], cross section stat. unc. [pb], maximum event weight, subprocess id
      outFile << xsec << " " << xsecErr << " " << wMax << " 81" << std::endl;
      outFile << "</init>" << std::endl;
    }

    // Read event line
    if (line.rfind("<event>")!=std::string::npos) {
      int nPar, d;
      double weight, scale, alphaEM, alphaS;
      if (not getline(inFile, line) || not (std::istringstream(line) >> nPar >> d >> weight >> scale >> alphaEM >> alphaS) || nPar <= 0)
        throw std::logic_error("[convert_SCLHE2LHE] Failed to parse event line: "+line);

      // Particle tuple: pdg id, status, mother index (1, 2), color flow tag (1, 2), 4-momentum, proper lifetime, spin, has daughter
      std::vector<std::tuple<int, int, int, int, int, int, ROOT::Math::PxPyPzMVector, double, double, bool>> parV(nPar);

      // Read particle lines
      for (size_t iPar=0; iPar < nPar; iPar++) {
        int pdgId, status, momI1, momI2, cl1, cl2;
        double px, py, pz, en, mass, ctau, spin;
        if (not getline(inFile, line) || not (std::istringstream(line) >> pdgId >> status >> momI1 >> momI2 >> cl1 >> cl2 >> px >> py >> pz >> en >> mass >> ctau >> spin) || momI1 > iPar || momI2 > iPar)
          throw std::logic_error("[convert_SCLHE2LHE] Failed to parse particle line: "+line);
        // Tauola fix (tau mother pdgId: photon --> electron)
        if (std::abs(pdgId) == 15)
          for (const auto& m : {momI1-1, momI2-1})
            if (m >= 0 && std::get<0>(parV[m]) == 22)
              std::get<0>(parV[m]) = m+1==momI1 ? 11 : -11;
        // Store particle info
        parV[iPar] = {pdgId, status, momI1, momI2, cl1, cl2, ROOT::Math::PxPyPzMVector(px, py, pz, mass), ctau, spin, false};
        for (int iMom=momI1; iMom>0 && iMom<=std::max(momI1, momI2); iMom++)
          std::get<9>(parV[iMom-1]) = true;
      }

      // PYTHIA fix (particle status --> -9 (beam), -1 (incoming), 2 (intermediate) and 1 (final))
      for (auto& pV : parV) {
        auto& [pdgId, status, momI1, momI2, cl1, cl2, p, ctau, spin, hasDaughter] = pV;
        if ((pdgId>1E9 || pdgId==2212) && momI1 == 0)
          status = -9;
        else if (not hasDaughter)
          status = 1;
        else if (momI1 == 0)
          status = -1;
        else if (std::abs(status) < 2)
          status = 2;
      }

      // Add event info
      outFile << "<event>" << std::endl;
      outFile << std::fixed << std::setprecision(8) << std::scientific;
      //# particles, subprocess id, event weight, event scale, alpha_em, alpha_s
      outFile << parV.size() << " 81 " << weight << " " << scale << " " << alphaEM << " " << alphaS << std::endl;
      for (const auto& pV : parV) {
        const auto& [pdgId, status, momI1, momI2, cl1, cl2, p, ctau, spin, hasDaughter] = pV;
        //particle: pdg id, status, mother index (1, 2), color flow tag (1, 2), (px, py, pz, energy, mass [GeV]), proper lifetime [mm], spin
        outFile << pdgId << " " << status << " " << momI1 << " " << momI2 << " " << cl1 << " " << cl2 << " ";
        outFile << std::fixed << std::setprecision(10) << std::scientific << p.Px() << " " << p.Py() << " " << p.Pz() << " " << p.E() << " " << p.M() << " ";
        outFile << std::fixed << std::setprecision(4)  << std::scientific << ctau << " " << spin << std::endl;
      }
      outFile << "</event>" << std::endl;
      nEvt++;
    }
  }

  outFile << "</LesHouchesEvents>" << std::endl;
  outFile.close();
  inFile.close();
  std::cout << nEvt << " events written in " << outFileName << std::endl;
};


int main(int argc, char *argv[])
{
  if (argc != 2) {
    std::cout << "Invalid input parameters!" << std::endl;
    std::cout << "Usage: ./convert_SCLHE2LHE <INPUT_FILE>" << std::endl;
    return 1;
  }
  convert_SCLHE2LHE(std::string(argv[1]));
  return 0;
};

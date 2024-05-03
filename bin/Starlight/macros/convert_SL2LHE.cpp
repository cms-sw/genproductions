#include <iostream>
#include <fstream>
#include "Math/Vector4D.h"
#include "TDatabasePDG.h"


int getMomPdgID(const int& chnId)
{
  // Derive mother PDG ID from channel PID
  if (chnId == 113 || chnId == 113011 || chnId == 113013 || chnId == 913)
    return 113; // rho0
  else if (chnId == 223 || chnId == 223211111)
    return 223; // omega
  else if (chnId == 443011 || chnId == 443013)
    return 443; // J/psi
  else if (chnId == 444011 || chnId == 444013)
    return 100443; // psi(2S)
  else if (chnId == 553011 || chnId == 553013)
    return 553; // Y(1S)
  else if (chnId == 554011 || chnId == 554013)
    return 100553; // Y(2S)
  else if (chnId == 555011 || chnId == 555013)
    return 200553; // Y(3S)
  else if (chnId == 9010221 || chnId == 225 || chnId == 335 || chnId == 115)
    return chnId; // f0(975) , f2(1270) , f2(1525) , a2(1320)
  else if (chnId == 221 || chnId == 331 || chnId == 441 || chnId == 333)
    return chnId; // eta , eta' , etac , phi
  return 0;
};


void convert_SL2LHE(const std::string& inFileName, const double& beamE1, const double& beamE2, const int& chnId)
{
  // Open input file
  std::ifstream inFile(inFileName);
  if (not inFile.is_open())
    throw std::logic_error("[convert_SL2LHE] Failed to open input file: "+inFileName);

  // Extract cross section
  double fidxsec(1), totxsec(3);
  std::ifstream xsecFile(inFileName.substr(0, inFileName.rfind("/")+1)+"xsec.out");
  if (xsecFile.is_open())
    xsecFile >> fidxsec >> totxsec;

  // Create output file
  const auto outFileName = inFileName.substr(0, inFileName.rfind(".")).substr(inFileName.rfind("/")+1) + ".lhe";
  std::ofstream outFile(outFileName);
  if (not outFile.is_open())
    throw std::logic_error("[convert_SL2LHE] Failed to open output file: "+outFileName);

  std::cout << "Converting STARlight output to LHE format" << std::endl;

  // Add header
  outFile << "<LesHouchesEvents version=\"3.0\">" << std::endl;
  outFile << "<!-- " << std::endl << " #Converted from STARLIGHT generator output " << std::endl << "-->" << std::endl;
  outFile << "<header>" << std::endl << "</header>" << std::endl;

  // Put generic initialization-level info since STARLIGHT doesn't save this
  outFile << "<init>" << std::endl;
  outFile << std::fixed << std::setprecision(8) << std::scientific;
  //LHE format: https://arxiv.org/pdf/hep-ph/0109068.pdf
  //beam pdg id (1, 2), beam energy [GeV] (1, 2), PDF author group (1, 2), PDF set id (1, 2), weight strategy, # subprocesses
  outFile << "2212 2212 " << beamE1 << " " << beamE2 << " 0 0 0 0 3 1" << std::endl;
  //cross section [pb], cross section stat. unc. [pb], maximum event weight, subprocess id
  outFile << fidxsec << " " << 0.0 << " " << totxsec << " 81" << std::endl;
  outFile << "</init>" << std::endl;

  int nEvt(0);
  std::string line, label;
  const auto momPdgId = getMomPdgID(chnId);
  std::unique_ptr<TDatabasePDG> dataPDG(TDatabasePDG::Instance());

  // Read input file
  while (getline(inFile, line)) {
    // Read event line
    int iEvt, nTrk, nVtx, iTrk(0), iVtx(0), jMom(1);
    // EVENT: n ntracks nvertices
	if (not (std::istringstream(line) >> label >> iEvt >> nTrk >> nVtx) ||
        label != "EVENT:")
      throw std::logic_error("[convert_SL2LHE] Failed to parse event line: "+line);

    // Particle tuple: pdg id, status, mother index, 4-momentum
    std::vector<std::tuple<int, int, int, ROOT::Math::PxPyPzMVector>> parV;
    parV.reserve(nVtx + nTrk);
    double scale(-1);

    while (iVtx < nVtx && getline(inFile, line)) {
      // Read vertex line
      std::istringstream stream(line);
      if (not (stream >> label))
        throw std::logic_error("[convert_SL2LHE] Failed to parse line: "+line);
      if (label == "GAMMAENERGIES:")
        stream >> scale;
      if (label != "VERTEX:")
        continue;

      double a;
      int vtxN, iMom, nDau, b;
      // VERTEX: x y z t nv nproc nparent ndaughters
      if (not (stream >> a >> a >> a >> a >> vtxN >> b >> iMom >> nDau) || vtxN != iVtx+1)
        throw std::logic_error("[convert_SL2LHE] Failed to parse vertex line: "+line);

      const bool addMom = (momPdgId > 0 && iMom == 0);
      const auto& momI = addMom ? jMom : iMom;
      if (addMom)
        parV.emplace_back(momPdgId, 2, 0, ROOT::Math::PxPyPzMVector(0,0,0,0));

      // Read track lines
      for (int iDau=0; iDau<nDau; iDau++, iTrk++) {
        // TRACK: GPID px py pz nev ntr stopv PDGPID
        double px, py, pz;
        int dauN, evtN, vtxN, pdgId;
        if (not getline(inFile, line) ||
            not (std::istringstream(line) >> label >> b >> px >> py >> pz >> evtN >> dauN >> vtxN >> pdgId) ||
            label != "TRACK:" ||
            dauN != iDau ||
            vtxN != iVtx ||
            evtN != iEvt)
          throw std::logic_error("[convert_SL2LHE] Failed to parse track line: "+line);
        
        const auto& par = dataPDG->GetParticle(pdgId);
        if (not par)
          throw std::logic_error("[convert_SL2LHE] Invalid PDG ID for track line: "+line);
        parV.emplace_back(pdgId, 1, momI, ROOT::Math::PxPyPzMVector(px, py, pz, par->Mass()));
        if (addMom)
          std::get<3>(parV[momI-1]) += std::get<3>(parV.back());
      }

      if (addMom)
        jMom += nDau+1;
      iVtx++;
    }
    if (iTrk != nTrk)
      throw std::logic_error("[convert_SL2LHE] Failed to find all tracks in event");

    outFile << "<event>" << std::endl;
    outFile << std::fixed << std::setprecision(8) << std::scientific;
    //# particles, subprocess id, event weight, event scale, alpha_em, alpha_s
    outFile << parV.size() << " 81 " << 1.0 << " " << scale << " " << -1.0 << " " << -1.0 << std::endl;
    outFile << std::fixed << std::setprecision(10) << std::scientific;
    for (const auto& pV : parV) {
      const auto& [pdgId, status, momI, p] = pV;
      //particle: pdg id, status, mother index (1, 2), color flow tag (1, 2), (px, py, pz, energy, mass [GeV]), proper lifetime [mm], spin
      outFile << pdgId << " " << status << " " << momI << " 0 0 0 " << p.Px() << " " << p.Py() << " " << p.Pz() << " " << p.E() << " " << p.M() << " 0.0000e+00 9.0000e+00" << std::endl;
    }
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
  if (argc != 5) {
    std::cout << "Invalid input parameters!" << std::endl;
    std::cout << "Usage: ./convert_SL2LHE <INPUT_FILE> <BEAM_1_E> <BEAM_2_E> <CHANNEL_PID>" << std::endl;
    return 1;
  }
  convert_SL2LHE(std::string(argv[1]), std::atof(argv[2]), std::atof(argv[3]), std::atoi(argv[4]));
  return 0;
};

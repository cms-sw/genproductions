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
  //beam particle 1, 2, beam energy 1, 2, author group, beam 1, 2, PDFSET beam 1, 2,
  outFile << "22 " << "22 " << beamE1 << " " << beamE2 << " 0 " << "0 " << "0 " << "0 " << "3 " << "1" << std::endl;
  outFile << "1.0 " << "0.0 " << "3.0 " << "81" << std::endl;
  outFile << "</init>" << std::endl;

  int nEvt(0);
  std::string line, label;
  const auto momPdgId = getMomPdgID(chnId);
  std::unique_ptr<TDatabasePDG> dataPDG(TDatabasePDG::Instance());
  outFile.precision(10);

  // Read input file
  while (getline(inFile, line)) {
    // Read event line
    int iEvt, nTrk, nVtx, iTrk(0), iVtx(0), jMom(1);
    // EVENT: n ntracks nvertices
	if (not (std::istringstream(line) >> label >> iEvt >> nTrk >> nVtx) ||
        label != "EVENT:")
      throw std::logic_error("[convert_SL2LHE] Failed to parse event line: "+line);

    std::vector<std::string> oLines;
    oLines.reserve(nTrk + nVtx);

    while (iVtx < nVtx && getline(inFile, line)) {
      // Read vertex line
      std::istringstream stream(line);
      if (not (stream >> label))
        throw std::logic_error("[convert_SL2LHE] Failed to parse line: "+line);
      else if (label != "VERTEX:")
        continue;

      double a;
      int vtxN, iMom, nDau, b;
      // VERTEX: x y z t nv nproc nparent ndaughters
      if (not (stream >> a >> a >> a >> a >> vtxN >> b >> iMom >> nDau) || vtxN != iVtx+1)
        throw std::logic_error("[convert_SL2LHE] Failed to parse vertex line: "+line);

      ROOT::Math::PxPyPzMVector momP(0,0,0,0);
      const bool addMom = (momPdgId > 0 && iMom == 0);
      const auto& momI = addMom ? jMom : iMom;
      std::vector<std::string> pLines(nDau + addMom);

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
        ROOT::Math::PxPyPzMVector dauP(px, py, pz, par->Mass());
        if (addMom)
          momP += dauP;

        std::ostringstream oLine;
        oLine << pdgId << " 1 " << momI << " " << momI << " 0 0 " << px << " " << py << " " << pz << " " << dauP.E() << " " << dauP.M() << " 0.0 9.0" << std::endl;
        pLines[iDau + addMom] = oLine.str();
      }

      if (addMom) {
        std::ostringstream oLine;
        oLine << momPdgId << " 2 0 0 0 0 " << momP.Px() << " " << momP.Py() << " " << momP.Pz() << " " << momP.E() << " " << momP.M() << " 0.0 9.0" << std::endl;
        pLines[0] = oLine.str();
        jMom += nDau+1;
      }

      oLines.insert(oLines.end(), pLines.begin(), pLines.end());
      iVtx++;
    }

    if (iTrk != nTrk)
      throw std::logic_error("[convert_SL2LHE] Failed to find all tracks in event");

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
  if (argc != 5) {
    std::cout << "Invalid input parameters!" << std::endl;
    std::cout << "Usage: ./convert_SL2LHE <INPUT_FILE> <BEAM_1_E> <BEAM_2_E> <CHANNEL_PID>" << std::endl;
    return 1;
  }
  convert_SL2LHE(std::string(argv[1]), std::atof(argv[2]), std::atof(argv[3]), std::atoi(argv[4]));
  return 0;
};

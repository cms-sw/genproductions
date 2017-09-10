#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/flush.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include "zlib.h"
#include <stdlib.h>
#include "TROOT.h"
#include "TMath.h"
#include "TLorentzVector.h"

int main(int argc, char** argv)
{
    if(argc != 3) {
        std::cout << ">>>LHE_filter.cpp::Usage:   " << argv[0] << "   initialFile.lhe   outputFile.lhe" << std::endl;
        return -1;
    }

    char* initialFileName = argv[1];
    char* outFileName = argv[2];

    std::cout << "initialFileName = " << initialFileName << std::endl;

    bool readEvent = false;
    int eventIt=0;
    int eventIt_2=0;

    double ctau_; // proper lifetime ctau (in mm)
    double spin_; // cosine of the angle btwn the spin vector and its 3-momentum, in the lab frame
    signed long pdgid_; // PDG numbering of the particle
    signed int statuscode_; // status code (-1 for initial state, 2 intermediate state, 1 final state)
    unsigned int mothup1_; // first mother index
    unsigned int mothup2_; // second mother index
    double px;
    double py;
    double pz;
    double energy;
    signed int color1; // color 1
    signed int color2; // color 2
    TLorentzVector L_1; // First lepton
    TLorentzVector L_2; // Second lepton
    bool b_first = false;
    bool b_second = false;

    int n_events_in=0;
    int n_events_out=0;
    int counter_lines_event=0;

    // open lhe file
    std::ifstream initialFile(initialFileName, std::ios::in);
    std::ifstream initialFile_2(initialFileName, std::ios::in);
    std::ofstream outFile200("test_200_600.lhe" , std::ios::out);
    std::ofstream outFile600("test_600_1200.lhe" , std::ios::out);
    std::ofstream outFile1200("test_1200_2500.lhe" , std::ios::out);
    std::ofstream outFile2500("test_2500_4000.lhe" , std::ios::out);
    std::ofstream outFile4000("test_4000.lhe" , std::ios::out);

    std::string line;
    std::string line_2;

    bool header=true;

    bool write_LHE_file=true;

    while(!initialFile.eof()) {
        getline(initialFile, line);

        if(line == "</LesHouchesEvents>") {
            outFile200 << line << std::endl;
            outFile600 << line << std::endl;
            outFile1200 << line << std::endl;
            outFile2500 << line << std::endl;
            outFile4000 << line << std::endl;
            break;
        }

        if(header) {
            outFile200 << line << std::endl;
            outFile600 << line << std::endl;
            outFile1200 << line << std::endl;
            outFile2500 << line << std::endl;
            outFile4000 << line << std::endl;
        }

        if(line == "</init>")header=false;

        if (line == "<event>") {
            readEvent = true;

            counter_lines_event=0;

            b_first = false;
            b_second = false;
            L_1.SetPxPyPzE(0.,0.,0.,0.);
            L_2.SetPxPyPzE(0.,0.,0.,0.);

            continue;
        }

        if( line == "</event>" ) {
            eventIt++;

            n_events_in++;
            readEvent = false;

            if(write_LHE_file){
                //filter on one muon and one electron with M(e,mu) in (400.,600.)
                if(b_first and b_second and (L_1 + L_2).M() > 200. and (L_1 + L_2).M() < 600.) {
                    n_events_out++;
                    while(!initialFile_2.eof()) {
                        getline(initialFile_2, line_2);

                        if (line_2 == "<event>") {
                            eventIt_2++;
                        }

                        if(eventIt_2==eventIt) {
                            outFile200 << line_2 << std::endl;

                        }

                        if (line_2 == "</event>") {
                            if(eventIt_2==eventIt) {
                                break;
                            }
                        }
                    }
                } else if (b_first and b_second and (L_1 + L_2).M() > 600. and (L_1 + L_2).M() < 1200.) {
                    n_events_out++;
                    while(!initialFile_2.eof()) {
                        getline(initialFile_2, line_2);

                        if (line_2 == "<event>") {
                            eventIt_2++;
                        }

                        if(eventIt_2==eventIt) {
                            outFile600 << line_2 << std::endl;

                        }

                        if (line_2 == "</event>") {
                            if(eventIt_2==eventIt) {
                                break;
                            }
                        }
                    }
                } else if (b_first and b_second and (L_1 + L_2).M() > 1200. and (L_1 + L_2).M() < 2500.) {
                    n_events_out++;
                    while(!initialFile_2.eof()) {
                        getline(initialFile_2, line_2);

                        if (line_2 == "<event>") {
                            eventIt_2++;
                        }

                        if(eventIt_2==eventIt) {
                            outFile1200 << line_2 << std::endl;

                        }

                        if (line_2 == "</event>") {
                            if(eventIt_2==eventIt) {
                                break;
                            }
                        }
                    }
                } else if (b_first and b_second and (L_1 + L_2).M() > 2500. and (L_1 + L_2).M() < 4000.) {
                    n_events_out++;
                    while(!initialFile_2.eof()) {
                        getline(initialFile_2, line_2);

                        if (line_2 == "<event>") {
                            eventIt_2++;
                        }

                        if(eventIt_2==eventIt) {
                            outFile2500 << line_2 << std::endl;

                        }

                        if (line_2 == "</event>") {
                            if(eventIt_2==eventIt) {
                                break;
                            }
                        }
                    }
                } else if (b_first and b_second and (L_1 + L_2).M() > 4000.) {
                    n_events_out++;
                    while(!initialFile_2.eof()) {
                        getline(initialFile_2, line_2);

                        if (line_2 == "<event>") {
                            eventIt_2++;
                        }

                        if(eventIt_2==eventIt) {
                            outFile4000 << line_2 << std::endl;

                        }

                        if (line_2 == "</event>") {
                            if(eventIt_2==eventIt) {
                                break;
                            }
                        }
                    }
                }
            }
        }

        if(readEvent) {
            counter_lines_event++;
            std::stringstream str;
            str << line;

            str >> pdgid_;
            str >> statuscode_;
            str >> mothup1_;
            str >> mothup2_;

            if(counter_lines_event == 1)continue;

            str >> color1;
            str >> color2;
            str >> px;
            str >> py;
            str >> pz;
            str >> energy;
            str >> ctau_;
            str >> spin_;

            if((TMath::Abs(pdgid_) == 11 or TMath::Abs(pdgid_) == 13 or TMath::Abs(pdgid_) == 15) and !b_first) {
                b_first = true;
                L_1.SetPxPyPzE(px,py,pz,energy);
                continue;
            }

            if((TMath::Abs(pdgid_) == 11 or TMath::Abs(pdgid_) == 13 or TMath::Abs(pdgid_) == 15) and !b_second) {
                b_second = true;
                L_2.SetPxPyPzE(px,py,pz,energy);
            }
        }

        // if (n_events_out % 1000 == 0) std::cout << n_events_in << std::endl;
        // if (n_events_out==2500) {
            // outFile500 << "</LesHouchesEvents>" << std::endl;
            // outFile800 << "</LesHouchesEvents>" << std::endl;
            // outFile1200 << "</LesHouchesEvents>" << std::endl;
            // outFile1800 << "</LesHouchesEvents>" << std::endl;
            // break;
        // }
    }

    std::cout << "events input LHE: " << n_events_in << std::endl;
    std::cout << "events output LHE: " << n_events_out << std::endl;
    return 0;
}




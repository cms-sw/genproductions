#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>
#include <string>
#include <iostream>

namespace {


  inline void ninja_config_overview()
  {
    std::cout << "ninja-config: prints information on the configuration\n"
              << "              of your Ninja installation\n" << std::endl;
  }


  inline void ninja_config_usage()
  {
    std::cout << "USAGE: ninja-config [options]" << std::endl;
    std::cout << std::endl;
    std::cout << "Called without options it shows all the available \n"
              << "information.  With one optional argument it prints what is\n"
              << "requested."
              << std::endl;
    std::cout << std::endl;
    std::cout << "Available options are:" << std::endl;
    std::cout << "  -quadsupport,--quadsupport,-quadninja,--quadninja:"
              << std::endl;
    std::cout << "      checks quadninja support and prints true or false"
              << std::endl;
    std::cout << "  -v,--version:" << std::endl;
    std::cout << "      prints the version of ninja in the format x.y.z"
              << std::endl;
    std::cout << "  -h,--help:" << std::endl;
    std::cout << "      prints this message"
              << std::endl;
  }


  inline void ninja_config_quadsupport()
  {
#ifdef QUADNINJA
    std::cout << "true" << std::endl;
#else
    std::cout << "false" << std::endl;
#endif
  }


  inline void ninja_config_version()
  {
    std::cout << VERSION << std::endl;
  }


  inline void ninja_config_version_info()
  {
    std::cout << "ninja-version: " << VERSION << std::endl;
  }


  inline void ninja_config_ninja_precision()
  {
#ifdef NINJA_QUADRUPLE 
    std::cout << "ninja-precision: quadruple" << std::endl;
#else
    std::cout << "ninja-precision: double" << std::endl;
#endif
  }


  inline void ninja_config_quadninja_precision()
  {
#ifdef QUADNINJA 
    std::cout << "quadninja-precision: quadruple" << std::endl;
#else
    std::cout << "quadninja-precision: none" << std::endl;
#endif
  }


  inline void ninja_config_higher_rank()
  {
#ifdef NINJA_X1RANK
    std::cout << "higher-rank support: yes" << std::endl;
#else
    std::cout << "higher-rank support: no" << std::endl;
#endif
  }


  inline void ninja_config_oneloop()
  {
#ifdef NINJA_USE_ONELOOP
    std::cout << "oneloop interface: yes" << std::endl;
# ifdef NINJA_USE_ONELOOP_WITH_CACHE
    std::cout << "oneloop cache: yes" << std::endl;
# else
    std::cout << "oneloop cache: no" << std::endl;
# endif
#else
    std::cout << "oneloop interface: no" << std::endl;
#endif
  }


  inline void ninja_config_looptools()
  {
#ifdef NINJA_USE_LOOPTOOLS
    std::cout << "looptools interface: yes" << std::endl;
#else
    std::cout << "looptools interface: no" << std::endl;
#endif
  }


  void ninja_config_info()
  {
    ninja_config_version_info();
    ninja_config_ninja_precision();
    ninja_config_quadninja_precision();
    ninja_config_higher_rank();
    ninja_config_oneloop();
    ninja_config_looptools();
  }
  

  int ninja_config_exec(const std::vector<std::string> & args)
  {
    if (args.size() == 0) {

      ninja_config_info();
      return 0;
      
    } else if (args.size() == 1 &&
        (args[0] == "-quadsupport" ||
         args[0] == "--quadsupport" ||
         args[0] == "-quadninja" ||
         args[0] == "--quadninja")) {
      
      ninja_config_quadsupport();
      return 0;
      
    } else if (args.size() == 1 &&
        (args[0] == "-h" ||
         args[0] == "--help")) {

      ninja_config_overview();
      ninja_config_usage();
      return 0;
      
    } else if (args.size() == 1 &&
        (args[0] == "-v" ||
         args[0] == "--version")) {

      ninja_config_version();
      return 0;
      
    }

    if (args.size() > 1)
      std::cerr << "ninja-config Error: too many arguments. " << std::endl;
    else
      std::cerr << "ninja-config Error: unknown argument "
                << args[0] << std::endl;
    ninja_config_usage();
    return 1;
  }

}

int main(int argc, char ** argv)
{
  std::vector<std::string> args;

  if (argc>1)
    args.assign(argv+1, argv+argc);
  
  return ninja_config_exec(args);
}

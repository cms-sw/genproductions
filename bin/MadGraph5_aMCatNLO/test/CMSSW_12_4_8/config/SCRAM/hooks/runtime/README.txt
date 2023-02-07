###############################################################
####                     For runtime-hook                  ####
#Each line, with valid format, printed on STDOUT will be      #
#interpreted by SCRAM and runtime environment will be updated.#
#Valid formats are                                            #
#1. append, prepend, remove a PATH type variable e.g          #
#RUNTIME:path:prepend|append|remove:VARIABLE_NAME=VALUE       #
#2. Set a environment variable e.g.                           #
#RUNTIME:variable:VARIABLE_NAME=VALUE"                        #
###############################################################

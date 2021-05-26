#!/bin/sh

# Top-level script for RSat 3.01

if [ "x$1" = "x" ]; then
  echo "USAGE: SatELiteGTI <input CNF>"
  exit 1
fi

TMP=/tmp/SatElite_temp  #SatElite_temp #set this to the location of temporary files
SE=./SatElite           #set this to the executable of SatELite
RS=./rsat               #set this to the executable of RSat
INPUT=$1;
shift 

$SE $INPUT -s $TMP.cnf $TMP.vmap $TMP.elim
X=$?
if [ $X = 0 ]; then
  #SatElite terminated correctly
  $RS $TMP.cnf -q -r $TMP.result "$@"
  X=$?
  if [ $X = 20 ]; then
    #RSat must not have printed out result!
    #there is no point calling SatELite either, so print it here and exit
    echo "s UNSATISFIABLE"
    rm -f $TMP.cnf $TMP.vmap $TMP.elim $TMP.result
    exit 20
    #Don't call SatElite for model extension.
  elif [ $X != 10 ]; then
    #timeout/unknown, nothing to do, just clean up and exit.
    rm -f $TMP.cnf $TMP.vmap $TMP.elim $TMP.result
    exit $X
  fi 
  
  #SATISFIABLE, call SatElite for model extension
  $SE +ext $INPUT -s $TMP.result $TMP.vmap $TMP.elim "$@"
  X=$?
elif [ $X = 11 ]; then
  #SatElite died, RSat must take care of the rest
  $RS $INPUT "$@" -s #but we must force rsat to print out result here!!!
  X=$?
elif [ $X = 12 ]; then
  #SatElite prints out usage message
  #There is nothing to do here.
  X=0
fi

#we reach this line if SatELite died and Rsat took over or if SatELite just printed usage message
rm -f $TMP.cnf $TMP.vmap $TMP.elim $TMP.result
exit $X

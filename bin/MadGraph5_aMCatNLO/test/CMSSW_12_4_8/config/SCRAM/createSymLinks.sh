#!/bin/bash

src=$1                    #source directory to search directories in
des=$2                    #destination directory to create symlinks in
depth=$3                  #how deep in source directory we search
subdir=$4 || ""           #sub-directory to search. There is a special cases
                          # . mean find directory with same name as parent e.g. in LCG project we have PackageA/PackageA
linkdir=$5 || ""          #name of symlink to create
srcfilter=""
srcnfilter=""
if [ $# -gt 5 ]; then 
  shift 5
  while [ $# -gt 0 ] ; do #filter to match in src path. A dash "-" in front of filter means to ignore source those src patchs
    arg="$(echo "$1" | sed 's/^\s+//')"
    arg="$(echo "$1" | sed 's/\s*$//')"
    if [[ $arg =~ ^-(.+)$ ]]; then
      srcnfilter="${srcnfilter}${BASH_REMATCH[1]}|"
    elif [[ $arg =~ ^(\+|)(.+)$ ]]; then
      srcfilter="${srcfilter}${BASH_REMATCH[2]}|"
    fi
    shift
  done
fi
srcfilter="$(echo "$srcfilter" | sed 's/|$//')"
srcnfilter="$(echo "$srcnfilter" | sed 's/|$//')"

function getSubDir () {
  local dir=$1
  local sdir=$2
  if [ "X$sdir" = "X" ] ; then return; fi
  if [ "X$sdir" = "X." ] ; then sdir="/`basename $dir`" ; else sdir="/${sdir}" ; fi
  echo "$sdir"
}

if [ -d "$src" ] ; then
  for dir in $(find $src -maxdepth $depth -mindepth $depth -name "*" -type d); do
    if [[ $dir =~ ^\. ]]; then continue; fi
    if [ ! -z "$srcnfilter" ] && [[ $dir =~ $srcnfilter ]]; then continue; fi
    if [ ! -z "$srcfilter" ] && [[ ! $dir =~ $srcfilter ]]; then continue; fi
    rpath=$dir
    rpath="$(echo "$rpath" | sed "s|${src}/*||")"
    sdir=$(getSubDir "$dir" "$subdir")
    ldir=$(getSubDir "$dir" "$linkdir")
    slink="${des}/${rpath}${ldir}"
    if [ -d "${dir}${sdir}" ]; then
      if [ "$des" = python ] &&  [ -d $slink ]; then rm -rf $slink; fi
      slinkdir=$(dirname "$slink")
      ldir="$slinkdir"
      ldir="$(echo "$ldir" | sed -E 's/[a-zA-Z0-9_-]+/../g')"
      if [ ! -h "$slink" ]; then
        [ -d $slinkdir ] || mkdir -p $slinkdir; ln -s ${ldir}/${dir}${sdir} $slink
        printf "  ${dir}${sdir} -> $slink\n"
      fi
    elif [ "$des" = python ] &&  [ ! -d $slink ]; then rm -f $slink && mkdir -p $slink
    fi
  done
fi

if [ -d "$des" ] ; then
 declare -A rm
 declare -A ok
 for d in $(find $des -name "*" -type l) ; do
  d1=$d
  d1="$(echo "$d1" | sed 's/\/[^\/]+$//')"
  if [ ! -e  $d ]; then
    unlink $d
    rm[$d1]=1
  else
    ok[$d1]=1
  fi
  done
  for K in "${!ok[@]}"; do unset rm[K]; done
  del=${rm[*]}
  if [[ ! $del =~ ^\s*$ ]]; then rm -rf $del; fi
fi

#!/bin/bash
TMPDIR=$(/bin/pwd)
SOURCE_EXTENSIONS=""
IGNORE_EXTENSIONS="f f77"
while [ "$#" != 0 ]; do
  case "$1" in
    -t | --tmpdir ) shift ; TMPDIR=$1; shift;;
    -e | --extensions ) shift ; SOURCE_FILES_EXT=$1 ; shift ;;
    -i | --ignore-extensions ) shift ; IGNORE_EXTENSIONS=$1; shift ;;
    * ) break ;;
  esac
done

cd $CMSSW_BASE
if [ -z $TMPDIR ] ; then TMPDIR=$(/bin/pwd) ; fi
if [ ! -f $TMPDIR/uses.out ] ; then
  mkdir -p $TMPDIR
  gunzip -qc $CMSSW_RELEASE_BASE/etc/dependencies/uses.out.gz > $TMPDIR/uses.out
fi

PKG_REGEX_FILE=$TMPDIR/package_match.regex
rm -f ${PKG_REGEX_FILE}
touch $TMPDIR/package_match.regex
if [ "X$@" = "X" ] ; then
  for pkg in $(find src -mindepth 2 -maxdepth 2 -type d -follow | grep -v '/.git/' | sed 's|^src/||') ; do
    echo "^${pkg}/" >> ${PKG_REGEX_FILE}
  done
else
  for pkg in $(echo $@ | sed 's|^src/||;s|^/*||;s|/*$||') ; do
    echo "^${pkg}/" >> ${PKG_REGEX_FILE}
  done
fi
if [ "X$SOURCE_EXTENSIONS" = "X" ] ; then
  SOURCE_EXTENSIONS=$(sed 's| .*||;s|^.*\.||' $TMPDIR/uses.out | grep -v '^$' | sort -u)
fi
if [ "X${IGNORE_EXTENSIONS}" != "X" ] ; then
  IGNORE_EXTENSION_EXP=$(echo $IGNORE_EXTENSIONS | tr ' ' '\n' | grep -v '^$' | tr '\n' '|' | sed 's/|*$//')
  SOURCE_EXTENSIONS=$(echo $SOURCE_EXTENSIONS | tr ' ' '\n' | grep -v '^$'| grep -v -E -i "^(${IGNORE_EXTENSION_EXP})"'$' | tr '\n' '|' | sed 's/|*$//')
fi
grep -E '^[^ ]*\.('${SOURCE_EXTENSIONS}') ' $TMPDIR/uses.out | tr ' ' '\n' | sort -u > $TMPDIR/uses-sel.out
ALL_EXTS=$(sed 's|^.*\.||' $TMPDIR/uses-sel.out | sort -u | tr '\n' '|' | sed 's/|*$//;s/^|*//')
(cd src; git diff --name-only $CMSSW_VERSION | grep -f ${PKG_REGEX_FILE}) > $TMPDIR/selected-source-files.txt.tmp
grep -f ${PKG_REGEX_FILE} $TMPDIR/uses-sel.out >> $TMPDIR/selected-source-files.txt.tmp
grep -E '^.*\.('${ALL_EXTS}')$' $TMPDIR/selected-source-files.txt.tmp | sort -u > $TMPDIR/selected-source-files.txt
echo "SELECTED_FILES_LIST=$TMPDIR/selected-source-files.txt"
echo "SELECTED_FILES_EXT=$(sed 's|^.*\.||' $TMPDIR/selected-source-files.txt | sort -u | tr '\n' ' ')"
rm -f ${PKG_REGEX_FILE} $TMPDIR/uses-sel.out $TMPDIR/selected-source-files.txt.tmp

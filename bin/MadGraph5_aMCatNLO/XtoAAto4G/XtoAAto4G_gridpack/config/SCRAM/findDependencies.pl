#!/usr/bin/env perl
# findDependencies.pl
#

use strict;
use File::Find ();
use Getopt::Long;
Getopt::Long::config('bundling_override');

my %options;
GetOptions(\%options,'rel=s', 'arch=s','scramroot=s');

my $rel=$options{'rel'};
my $scramarch=$options{'arch'} || $ENV{SCRAM_ARCH};
my $scramroot=$options{'scramroot'} || $ENV{SCRAMV1_ROOT};

if ($scramroot eq "")
{
  $scramroot=`sh -v scram arch 2>&1 |  grep 'SCRAMV1_ROOT=' | sed 's|;.*||;s|SCRAMV1_ROOT=||;s|"||g' | sed -e "s|'||g"`;
  chomp $scramroot;
}

# Set the variable $File::Find::dont_use_nlink if you're using AFS,
# since AFS cheats.

# for the convenience of &wanted calls, including -eval statements:
use vars qw/*name *dir *prune/;
*name   = *File::Find::name;
*dir    = *File::Find::dir;
*prune  = *File::Find::prune;

sub wanted;
sub doexec ($@);


use Cwd ();
my $cwd = Cwd::cwd();

my %uses;
my %usedby;
my @prod2src;
my $beginning="";

# Traverse desired filesystems
my $directory="${rel}/tmp";
File::Find::find({wanted => \&wanted}, $directory);

open F1,">${rel}/etc/dependencies/uses.out";
foreach my $key (keys %uses) {
    print F1 "$key $uses{$key}\n";
}
close F1;

open F1,">${rel}/etc/dependencies/usedby.out";
foreach my $key (keys %usedby) {
    print F1 "$key $usedby{$key}\n";
}
close F1;

open F1,">${rel}/etc/dependencies/prod2src.out";
foreach my $dep (@prod2src) {print F1 "$dep";}
close F1;

&pythonDeps($rel);
&buildFileDeps($rel, $scramarch, $scramroot);

exit;

sub wanted {
  if ($name=~/^.*(\.dep)\z/os){doexec(0, 'cat','{}');}
  elsif ($name=~/\/scram-prod2src[.]txt$/os){
    open F1, $name;
    push @prod2src, <F1>;
    close F1;
  }
}

sub doexec ($@) {
    my $ok = shift;
    my @command = @_; # copy so we don't try to s/// aliases to constants

    my $getnext=0;
    my $depname;
    open F1, $name;
    while (my $l=<F1>) {
    	chomp($l);
	last if ($l=~/^[^:]+ :\s*$/o);
	$l=~s/\s*\\$//o;
	my @sp1=split(' ',$l);
	next if ( scalar(@sp1) == 0);
	next if ( length($sp1[0]) < 4);
	my @sp2=split('/',$sp1[0]);
	my $tsp1="";
	my $foundsrc=0;
	foreach my $t (@sp2) {
	    $tsp1.="$t/" if ( $foundsrc==1);
	    $foundsrc=1 if ( $t eq "src" ); 
	}
	chop($tsp1);
	next if ( $tsp1 eq "");
	if ( $getnext==1 ) {
	    $depname=$tsp1;
	    $getnext=0;
	}
	else{
	    if ( $sp1[0] =~ /^tmp\//o ) {
		if ( $sp1[0] =~/(\.o|\.cc)\s*:$/o) {
		    $getnext=1;
		}
	    }
	    else{
		if ( $sp1[0] =~ /^src/o ) {
		    $uses{$depname}.="$tsp1 ";
		    $usedby{$tsp1}.="$depname ";
		}
	    }
	}
    }
    close F1;
    chdir $cwd; #sigh
    chdir $File::Find::dir;
    return !$?;
}

sub pythonDeps()
{
  my $rel=shift;
  my %cache=();
  foreach my $fname (`find ${rel}/src -name "*.py" -type f`)
  {
    chomp $fname;
    my $file=$fname;
    $file=~s/^${rel}\/+src\/+//o;
    if($fname!~/\/python\//o){next;}
    foreach my $line (`grep 'import ' $fname`)
    {
      chomp $line;
      if ($line=~/^\s*#/){next;}
      if ($line=~/^\s*from\s+([^\s]+)\s+import\s+/)
      {
        foreach my $x (&import2CMSSWDir($1,\%cache))
        {
          $cache{usedby}{$x}{$file}=1;
  	  $cache{uses}{$file}{$x}=1;
        }
      }
      elsif ($line=~/^\s*import\s+([^\s]+)\s*/)
      {
        foreach my $x (&import2CMSSWDir($1,\%cache))
        {
          $cache{usedby}{$x}{$file}=1;
	  $cache{uses}{$file}{$x}=1;
        }
      }
    }
  }
  foreach my $type ("uses","usedby")
  {  
    my $ref;
    open $ref,">${rel}/etc/dependencies/py${type}.out";
    foreach my $x (sort keys %{$cache{$type}}){print $ref "$x ",join(" ",sort keys %{$cache{$type}{$x}}),"\n";}
    close($ref);
  }
}

sub import2CMSSWDir()
{
  my ($str,$cache)=@_;
  my @pyfiles=();
  foreach my $s (split ',',$str)
  {
    $s=~s/\./\//og;
    if (exists $cache->{pymodule}{$s}){push @pyfiles,$cache->{pymodule}{$s};}
    elsif(!exists $cache->{noncmsmodule}{$s})
    {
      if (-f "${rel}/python/${s}.py")
      {
	$s=~/^([^\/]+\/+[^\/]+)\/+(.+)$/o;
	$cache->{pymodule}{$s}="${1}/python/${2}.py";
	push @pyfiles,"${1}/python/${2}.py";
      }
      else{$cache->{noncmsmodule}{$s}=1;}
    }
  }
  return @pyfiles;
}

sub buildFileDeps()
{
  my $rel=shift;
  my $arch=shift;
  my $scramroot=shift;

  unshift @INC,"$scramroot", "${scramroot}/src";
  my $pcache={};
  eval ("use Cache::CacheUtilities;");
  if(!$@){$pcache=&Cache::CacheUtilities::read("${rel}/.SCRAM/${arch}/ProjectCache.db.gz");}
  else{die "Unable to find Cache/CacheUtilities.pm PERL module.";}  
  my %cache=();
  foreach my $dir (sort keys %{$pcache->{BUILDTREE}})
  {
    if ($pcache->{BUILDTREE}{$dir}{SUFFIX} ne ""){next;}
    if (scalar(@{$pcache->{BUILDTREE}{$dir}{METABF}}) == 0){next;}
    my $bf=$pcache->{BUILDTREE}{$dir}{METABF}[0]; $bf=~s/src\///;
    $cache{dirs}{$dir}=$bf;
    my $pack=$dir;
    my $class=$pcache->{BUILDTREE}{$dir}{CLASS};
    if ($class=~/^(LIBRARY|CLASSLIB)$/){$pack=$pcache->{BUILDTREE}{$dir}{PARENT};}
    $cache{packs}{$pack}=$dir;
  }
  foreach my $dir (keys %{$cache{dirs}}){&updateBFDeps($dir,$pcache,\%cache,"");}
  foreach my $type ("uses","usedby")
  {  
    my $ref;
    open $ref,">${rel}/etc/dependencies/bf${type}.out";
    foreach my $x (sort keys %{$cache{$type}}){print $ref "$x ",join(" ",sort keys %{$cache{$type}{$x}}),"\n";}
    close($ref);
  }
}

sub updateBFDeps()
{
  my ($dir,$pcache,$cache)=@_;
  my $bf=$cache->{dirs}{$dir};
  if (exists $cache->{uses}{$bf}){return;}
  $cache->{uses}{$bf}={};
  foreach my $pack (keys %{$pcache->{BUILDTREE}{$dir}{RAWDATA}{DEPENDENCIES}})
  {
    if (exists $cache->{packs}{$pack})
    {
      my $xdata=$cache->{packs}{$pack};
      &updateBFDeps($xdata,$pcache,$cache);      
      $xdata=$cache->{dirs}{$xdata};
      $cache->{uses}{$bf}{$xdata}=1;
      $cache->{usedby}{$xdata}{$bf}=1;
      foreach my $xdep (keys %{$cache->{uses}{$xdata}})
      {
	$cache->{uses}{$bf}{$xdep}=1;
        $cache->{usedby}{$xdep}{$bf}=1;    
      }
    }
  }
}

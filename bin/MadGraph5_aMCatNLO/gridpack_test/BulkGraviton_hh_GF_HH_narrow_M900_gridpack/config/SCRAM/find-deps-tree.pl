#!/usr/bin/env perl

BEGIN{unshift @INC,$ENV{SCRAM_TOOL_HOME};}

use Cache::CacheUtilities;

my @conf=();

my $pack=shift;
my $dep=shift;

my $base=shift;  #CMSSW_BASE
push @conf,$base;

$base=shift || "";  #CMSSW_RELEASE_BASE
if ($base ne ""){push @conf,$base;}

$base=shift || "";  #FULL_RELEASE_BASE for patch releases
if ($base ne ""){push @conf,$base;}

my %cache=();
foreach my $p (@conf){&readDeps($p, \%cache);}
my $toolfile=$conf[0]."/.SCRAM/$ENV{SCRAM_ARCH}/ToolCache.db.gz";
my $cc=&Cache::CacheUtilities::read($toolfile);
foreach my $tool (keys %{$cc->{SETUP}})
{
  $cache{$tool}={};
  foreach my $d (@{$cc->{SETUP}{$tool}{USE}}){$cache{$tool}{$d}=1;}
}

foreach my $p (keys %cache)
{
  if ($p=~/^(.+)\/src$/)
  {
    my $pp=$1;
    if (!exists $cache{$pp}){$cache{$pp}={};}
    foreach my $d (keys %{$cache{$p}}){$cache{$pp}{$d}=1;}
    delete $cache{$p};
  }
}

if (!exists $cache{$pack}){print "Error: No such package/tool: $pack\n";}
if (!exists $cache{$dep}){print "Error: No such package/tool: $dep\n";}
my @chain=();
&findDeps(\%cache,$pack,$dep, \@chain);


sub readDeps()
{
  my ($release, $cache)=@_;
  my $cfile="${release}/.SCRAM/$ENV{SCRAM_ARCH}/ProjectCache.db.gz";
  if (!-f $cfile){return;}
  my $cc=&Cache::CacheUtilities::read($cfile);
  foreach my $dir (keys %{$cc->{BUILDTREE}})
  {
    if (-e $cache->{$dir}){next;}
    $cache->{$dir}={};
    foreach my $d (keys %{$cc->{BUILDTREE}{$dir}{RAWDATA}{DEPENDENCIES}}){$cache->{$dir}{$d}=1;}
  }
}

sub findDeps()
{
  my ($cache, $pack, $dep, $chain)=@_;
  push @$chain,$pack;
  foreach my $d (sort keys %{$cache{$pack}})
  {
    if ($d eq $dep){print "  ",join("->",@{$chain}),"->$d\n";}
    else
    {
      &findDeps($cache,$d,$dep, $chain);
      if (exists $cache{"${d}/src"}){&findDeps($cache,"${d}/src",$dep, $chain);}
    }
  }
  pop @$chain;
}

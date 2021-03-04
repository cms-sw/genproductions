#!/usr/bin/perl -w

################################################################################
# merge.pl
#  Richard Corke (richard.corke@thep.lu.se)
#
# Changed structure to not read entire files into memory
# 
# Based on merge.pl v1.2 by Michel Herquet (UCL-CP3)
# DESCRIPTION : script to merge to LHE events files
# USAGE :
# ./merge.pl eventfile1.lhe.gz eventfile2.lhe.gz ... result.lhe.gz banner.txt
# Note that result can be eventfile1, eventfile2, ...
# OUTPUT : merged file, banner
################################################################################

use Compress::Zlib;
use List::Util qw[min max];


###########################################################################
# Initialisation
###########################################################################
# Set tag names for later
my $begin_header = '<header>';
my $end_header   = '</header>';
my $begin_event  = '<event>';
my $end_event    = '</event>';
my $begin_init   = '<init>';
my $end_init     = '</init>';
my $event_norm   = 'EMPTY'; 


# Parse the command line arguments
if ( $#ARGV < 2 ) {
  die "This script must be called with at least three filenames as arguments!\n";
}
my $bannerfile = pop(@ARGV);
my $outfile = pop(@ARGV);


###########################################################################
# First pass - extract number of events and cross-sections
###########################################################################
foreach $infile (@ARGV) {
  $gzin = gzopen($infile, "r") || die ("Couldn't open file $infile\n");

  # No. events and cross-section from current file
  $noevents = $xsec = -1;

  # Storage for current file's init block
  @gzinit = ();

  # Keep track if we are in the init block or not
  $initblock = 0;

  # LHE version extracted from current file ; 1 by default
  $lhe_version = 1.0;

  while (1) {
    $gzbytes = $gzin->gzreadline($gzline);
    if ($gzbytes == -1) { die("Error reading from file $infile\n"); }
    if ($gzbytes == 0)  { last; }
    
    # Extract normalisation (sum or average) for proper weight handling 
    if ($event_norm eq 'EMPTY') {
        if ($gzline =~ s/\s*(.*)\s*=\s*event_norm.*\n/$1/) { $event_norm = $gzline; }
    }
    
    # Extract <MGGenerationInfo> information
    if ($initblock == 0) {
      if ($gzline =~ s/#  Number of Events\s*:\s*(.*)\n/$1/) { $noevents = $gzline; }
      if ($gzline =~ s/#  Integrated weight \(pb\)\s*:\s*(.*)\n/$1/) { $xsec = $gzline; }
      if ($gzline =~ s/\s*(.*)\s*=\s*lhe_version.*\n/$1/) { $lhe_version = $gzline; }

      # Check if we enter <init> block
      if ($gzline =~ m/$begin_init/) { $initblock++; next; }

    # Extract <init> information
    } else {
 
      # Check for end of <init> block
      if ($gzline =~ m/$end_init/) { last; }

      # Remove leading whitespace and split
      $gzline  =~ s/^\s+//;
      @gzparam = split(/\s+/, $gzline);
     
      # Skip the <generator> block introduced in LHE version 3
      if ($lhe_version >= 3 && $gzline =~ m/<generator/) {
        push(@gzinit, $gzline);
        next;
      }

      # Check <init> block line has right no. of entries
      if ($initblock == 1) {
        if ($#gzparam != 9) { die "Not right number of params in init"; }
      } else {
        if ($#gzparam != 3) { die "Not right number of params in init"; }
      }

      push(@gzinit, [ @gzparam ]);
      $initblock++;

    }
  }
  $gzin->gzclose();

  # Check the file contained all the information we need
  if ($noevents == -1 || $xsec == -1) {
    die("Couldn't extract No. Events / Cross-section from $infile")
  }

  # Store information for later
  push(@infiles, [ $infile, $noevents, $xsec, [ @gzinit ], $lhe_version ]);
}


###########################################################################
# Check/compute cross-sections, totals and unit weight
###########################################################################
$oldxsec        = $infiles[0][2];
@oldinit        = ($infiles[0][3][0]);
$oldlhe_version = $infiles[0][4];

# Form a composite init block by including any subprocess that 
# is present for any of the files. Take the first occurance 
# of the subprocess from all the files. 
my %uniqentries;
my $genblock;
foreach $infile (@infiles) {
  for($i=1; $i < scalar(@{$infile->[3]}); $i++) {
    if ($infile->[3][$i] =~ /^<generator/) {
        $genblock = $infile->[3][$i];
        next;
    }
    # Important to take first entry, as this is treated
    # differently in the next loop
    elsif (!exists $uniqentries{$infile->[3][$i][3]}) {
      $uniqentries{$infile->[3][$i][3]} = $infile->[3][$i];
    }
  }
}
# Maintain MadGraph's convention of reverse sorting by LPRUP
my @initentry = reverse sort { $a->[3] <=> $b->[3] } values %uniqentries;
push(@oldinit, @initentry);
push(@oldinit, $genblock);

$totevents = 0;  $totxsec = 0.0;
foreach $infile (@infiles) {
  print "Input file: $infile->[0]\n";
  print " No. Events = $infile->[1], Cross-section = $infile->[2], LHE version = $infile->[4]\n";

  # Check that cross sections do not differ too much
  $newxsec = $infile->[2];
  if (abs(($newxsec - $oldxsec) / $newxsec) > 0.05 ) {
    print " WARNING Cross sections do not agree with a 5\% precision!\n";
  }
  $oldxsec = $newxsec;

  $curlhe_version = $infile->[4];
  if (abs($curlhe_version - $oldlhe_version) > 0.001) {
    die("LHE version does not match");
  }

  @currinit = @{$infile->[3]};

  # Same number of entries on first line of <init> block?
  if ($#{$oldinit[0]} != $#{$currinit[0]}) { die("Init blocks do not match"); }

  # Create new init block (overwrite first file's init block data)
  for ($i = 1; $i <= $#oldinit; $i++) {
    # Match entry in init block based on LPRUP
    my @matchingsubprocess = ();
    if ($oldinit[$i] =~ /^<generator/) {
      next
    }
    for ($j = 1; $j <= $#currinit; $j++) {
      if ($currinit[$j] =~ /^<generator/) {
        next;
      }
      if (${oldinit[$i][3]} == ${currinit[$j][3]}) {
        push(@matchingsubprocess, @{$currinit[$j]});
      }
    }
    if (scalar(@matchingsubprocess) == 0) {
      next
    }

    if ($oldinit[$i] =~ /^<generator/) {
      if ($oldinit[$i] ne @matchingsubprocess) { die("Init blocks do not match"); } 
      next;
    }

    if ($oldinit[$i][3] != $matchingsubprocess[3]) { die("Init blocks do not match"); }

    print " xsecup = $matchingsubprocess[0], xerrup = $matchingsubprocess[1]\n";
    print " xmaxup = $matchingsubprocess[2], lprup = $matchingsubprocess[3]\n";

    # XSECUP = sum(xsecup * no.events) / tot.events
    # XERRUP = sqrt( sum(sigma^2 * no.events^2) ) / tot.events

    # Here we temporarily store:
    #  sum(xsecup * no.events)
    #  sum(sigma^2 * no.events^2)
    my $identicalentry = 1;
    for ($j = 0; $j <= $#matchingsubprocess; $j++) {
      if ($matchingsubprocess[$j] != $oldinit[$i][$j]) {
         $identicalentry = 0;
      }
    }
    if ($identicalentry) {
      $oldinit[$i][0] *= $infile->[1];
      $oldinit[$i][1] *= $oldinit[$i][1] * $infile->[1]**2;
    } else {
      $oldinit[$i][0] += ($matchingsubprocess[0] * $infile->[1]);
      $oldinit[$i][1] += $matchingsubprocess[1]**2 * $infile->[1]**2;
    }

    # XMAXUP = max(xmaxup)
    $oldinit[$i][2] = max($oldinit[$i][2], $matchingsubprocess[2]);
  }

  # Total number of events and total cross-section
  $totevents += $infile->[1];
  $totxsec += ($infile->[1] * $infile->[2]);

  print "\n";
}
print "\n";

# Finish calculation of XSECUP and XERRUP
for ($i = 1; $i <= $#oldinit; $i++) {
  if ($oldinit[$i] =~ /^<generator/) { next; }
  $oldinit[$i][0] /= $totevents;
  $oldinit[$i][0] = sprintf('%0.5E', $oldinit[$i][0]);

  $oldinit[$i][1] = sqrt($oldinit[$i][1]);
  $oldinit[$i][1] /= $totevents;
  $oldinit[$i][1] = sprintf('%0.5E', $oldinit[$i][1]);
}

# Finish calculation of total xsec and new unit weight
$totxsec /= $totevents;
$dispxsec = sprintf('%0.5E', $totxsec);

# Adjust unit weight according to normalization scheme (sum or average)
if (index(lc($event_norm), "average") != -1) {
    $uwgt = sprintf('%0.5E', $totxsec );
} elsif (index(lc($event_norm), "sum") != -1) {
    $uwgt = sprintf('%0.5E', $totxsec / $totevents);   
} else {
    die ("Unknown normalization mode \n");
}

# Display new information
print "Banner file: $bannerfile\n";
print "Output file: $outfile\n";
print " No. Events = $totevents, Cross-section = $dispxsec\n";
for ($i = 1; $i <= $#oldinit; $i++) {
  if ($oldinit[$i] =~ /^<generator/) { next; }
  print " xsecup = $oldinit[$i][0], xerrup = $oldinit[$i][1]\n";
  print " xmaxup = $oldinit[$i][2], lprup = $oldinit[$i][3]\n";
}
print "\n";
print " Unit weight = $uwgt\n";


###########################################################################
# Second pass - output file with new information
###########################################################################
# The first file is written out with changes to the header, init block
# and unit weight of each event.
# All events from other files are then written out with their unit weight
# changed.
###########################################################################

$gzout    = gzopen($outfile, "w") || die ("Couldn't open file $outfile\n");
open(BANNER, ">$bannerfile")      || die ("Couldn't open file $outfile\n");

$stage = 0;
$eventcount = 0;
foreach $infile (@infiles) {
  $gzin = gzopen($infile->[0], "r") || die ("Couldn't open file $infile\n");

  while (1) {
    $gzbytes = $gzin->gzreadline($gzline);
    if ($gzbytes == -1) { die("Error reading from file $infile\n"); }
    if ($gzbytes == 0)  { last; }

    # Pre-header
    if ($stage == 0) {
      if ($gzline =~ m/$begin_header/) { $stage++; }

    # Header (and output to banner)
    } elsif ($stage == 1) {
      $gzline =~ s/#  Integrated weight \(pb\)\s*:(.*)\n/#  Integrated weight (pb)  :  $dispxsec\n/;
      $gzline =~ s/#  Number of Events\s*:(.*)\n/#  Number of Events        :  $totevents\n/;
      $gzline =~ s/#  Unit wgt\s*:(.*)\n/#  Unit wgt                :  $uwgt\n/;

      if ($gzline =~ m/$end_header/)   { $stage++; }
      else { print BANNER $gzline; }

    # Init block
    } elsif ($stage == 2) {
      if ($gzline =~ m/$end_init/) {
        $gzline = "<init>\n";

        for ($i = 0; $i <= $#oldinit; $i++) {
          if ($oldinit[$i] =~ /^<generator/) {
            $gzline .= $oldinit[$i];
            next;
          }

          $gzline .= "  ";
          for ($j = 0; $j <= $#{$oldinit[$i]}; $j++) {
            $gzline .= "$oldinit[$i][$j] ";
          }
          $gzline .= "\n";
        }

        $gzline .= "</init>\n";
        
        $stage++;
      } else { next; }

    # Pre-event
    } elsif ($stage == 3) {
      if ($gzline =~ m/$begin_event/)  { $stage++; } else { next; }

    # Event information
    } elsif ($stage == 4) {
      $gzline  =~ s/^\s+//;
      @gzparam = split(/\s+/, $gzline);
      if ($#gzparam != 5) { die "Not right number of param in first line of event"; }
      # Keep weight sign from original LHE file
      $signed_uwgt = abs($uwgt);
      if ($gzparam[2] < 0) {
	  $signed_uwgt = -1 * $signed_uwgt;
      }
      $gzline = " $gzparam[0] $gzparam[1] $signed_uwgt $gzparam[3] $gzparam[4] $gzparam[5]\n";


      $stage++;

    # Event particles
    } elsif ($stage == 5) {
      if ($gzline =~ m/$end_event/)    { $stage = 3; $eventcount++ }

    }

    # Write out the line
    $gzout->gzwrite($gzline);
  }
}

# Write out closing tag and close
$gzout->gzwrite("</LesHouchesEvents>\n");
$gzout->gzclose();

print "Wrote $eventcount events\n";
exit(0);


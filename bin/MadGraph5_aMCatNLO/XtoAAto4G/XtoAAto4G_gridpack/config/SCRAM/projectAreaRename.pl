#!/usr/bin/env perl
use File::Basename;
my $olddir=shift || die "Missing old installation path";
my $newtop=shift || die "Missing current installation path";
my $arch=shift || die "Missing SCRAM arch";
my $dir=shift; 
if(!defined $dir){$dir=`/bin/pwd`; chomp $dir;}
my $rel=$dir;
while((!-d "${rel}/.SCRAM") && ($rel!~/^[\.\/]$/)){$rel=dirname($rel);}
if(!-d "${rel}/.SCRAM"){die "$dir is not a SCRAM-based project area.";}

if($olddir ne $newtop)
{
  my $qod=quotemeta($olddir);
  foreach my $d ("${rel}/.SCRAM/${arch}","${rel}/config")
  {
    foreach my $f (`find $d -type f`)
    {
      chomp $f;
      my $ftime=0; my $gzip=0;
      if ($f=~/.db.gz$/)
      {
        $gzip=1;
        my @s=stat($f);
        $ftime=$s[9];
        system("gunzip -S.gz $f");
        $f=~s/\.gz$//;
      }
      my $oref=undef; my $has_olddir=0;
      if(open($oref,">${f}.rename"))
      {
        my $iref=undef;
        if (open($iref,$f))
        {
          my $l;
          while($l=<$iref>)
          {
            if ($l=~/$qod/o)
            {
              $has_olddir++;
              $l=~s/$qod/$newtop/go;
            }
            print $oref $l;
          }
          close($iref);
        }
        else { die "Error: Unable to read file ${f}\n"; }
        close($oref);
      }
      else{die "Error: Unable to create file ${f}.rename\n";}
      if ($has_olddir==0){unlink("${f}.rename");}
      else
      {
        if ($ftime==0){my @s=stat($f); $ftime=$s[9];}
        system("mv ${f}.rename $f");
      }
      if ($gzip){system("gzip -S.gz $f"); $f="${f}.gz";}
      if ($ftime>0){utime $ftime,$ftime,$f;}
    }
  }
}


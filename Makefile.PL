
use 5.005;
use strict;
use POSIX;
use File::Find;
use ExtUtils::MakeMaker;
use IO::File;
use Config;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

my ($texmfRoot, $mfdir, $tfmdir, $texInputs, $kpsewhich, %pkFontHash, %find);

my $dos = (($Config{osname} eq 'dos') or ($Config{osname} eq 'win32'));

my $ver = `svnversion .`;
# if good, $ver might be one of: qw( 4168 4123:4167 4166M 4165S 4212:4164MS )
# if svnversion doesn't exist, $ver will be undef.  If there is no .svn info,
# $ver will be something like "export\n"
if (defined($ver) and
    ($ver =~ m/(\d+)\D*(\d*)/)) {
    my $v;
    if (($2 eq '') or
        ($1 > $2)) {
        $v = $1;
    } else {        # switched
        $v = $2;
    }
    print "Setting version $v into sgf2dg\n";
    my $e = 's/(\$Revision:\s*)\d*(\s*\$)/${1}' . $v . '${2}/'; # script
    system("perl -pi -e '$e' sgf2dg");
    # print `grep VERSION sgf2dg`;       # verify script worked
}


eval { require PDF::Create; };   # is this module available?
if ($@) {
    print "\nPDF::Create not available\n",
          "  I'll install Games::Go::Sgf2Dg, but the PDF converter (Dg2PDF) needs PDF::Create.\n",
          "  You can find PDF::Create in the same repository where you found\n",
          "  Games::Go::Sgf2Dg, or from http://search.cpan.org/\n\n";

} else {
    my $v = ($PDF::Create::VERSION =~ m/(^\d*\.\d*)/)[0];
    if (not defined($v)) {
        print("\n\n  Hmm, can't extract PDF::Create package version from $PDF::Create::VERSION.\n" .
                  "  There may be a more recent version at:\n\n" .
                  "      http://www.sourceforge.net/projects/perl-pdf.\n\n");
    } elsif ($v < 0.06) {
        print("\n\n  Note: your PDF::Create package is version $PDF::Create::VERSION.\n" .
                  "  You might want to pick up a more recent version from:\n\n" .
                  "      http://www.sourceforge.net/projects/perl-pdf.\n\n");
    }
}

eval { require PostScript::File; };   # is this module available?
if ($@) {
    print "\nPostScript::File not available\n",
          "  I'll install Games::Go::Sgf2Dg, but the PostScript converter (Dg2Ps) needs\n",
          "  PostScript::File.\n",
          "  You can find PostScript::File in the same repository where you found\n",
          "  Games::Go::Sgf2Dg, or from http://search.cpan.org/\n\n";

}

my @exe_files = ('sgf2dg');
if ($dos) {     # punt
    print "\nI'm sorry, but since this is a DOS platform, if you need sgfsplit, you'll\n",
          "  need to compile it yourself.  If you've got all the right tools, you may\n",
          "  be able to type 'make sgfsplit.exe'.\n\n";
} else {
    push(@exe_files, 'sgfsplit');
}

WriteMakefile(
    NAME                => 'Games::Go::Sgf2Dg', # package name
    VERSION_FROM        => 'sgf2dg',            # finds $VERSION
    EXE_FILES           => \@exe_files,
    OBJECT              => 'sgfsplit.o',        # only one .o for this non-perl program
    MAN1PODS            => {sgf2dg      => "\$(INST_MAN1DIR)/sgf2dg.1",
                            'sgfsplit.c' => "\$(INST_MAN1DIR)/sgfsplit.1",},
    ABSTRACT            => "sgf2dg (replaces sgf2tex) converts Smart Go Format (SGF) files to Go diagrams - includes the GOOE TeX fonts",
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (AUTHOR     => 'Dan Bump <bump@match.stanford.edu>, Reid Augustin <reid@hellosix.com>') : ()),
);

$| = 1;             # autoflush STDOUT from now on
editTexMakefile();      # edit Makefile in tex subdirectory
editMakefile();         # make changes to Makefile just built by WriteMakefile()


#########################################################
#
#       Subroutines
#
#########################################################

# add install_tex and fonts rules in the postamble
sub MY::postamble {
    return <<"TEX_TARGET";
install_tex :
	cd tex; \${MAKE} install

fonts :
	cd tex; \${MAKE} fonts

TEX_TARGET
}

sub editTexMakefile {
    # installing TeX stuff is a bit tricky - we don't know where it
    #   might be on this system.  try kpsewhich:

    my $v;
    eval { $v = getVariables(); };      # try to find TEXMF variables
    if ($@) {
        print ("\nUser Abort - TeX fonts and macros will not be installed during 'make install'.\n",
               "             However, you can still 'make install' for the rest of Sgf2Dg\n\n");
        exit(0);    # not an error
    }
    my $texmfRoot ='/usr/share/texmf';
    my $mfdir = "$texmfRoot/fonts/source/public/GOOE";
    my $tfmdir = "$texmfRoot/fonts/tfm/public/GOOE";
    my $pkfonts = '/var/lib/texmf/pk/ljfour/public/GOOE/go*pk';
    my $texinputs = '/usr/share/texmf/tex/GOOE';

    my $pkfonts = join("/go*pk ", @{$v->{PKFONTS}}) . "/go*pk";

    print "

Please check the variables below carefully.  They are currently
written into tex/Makefile, and will be used during 'make install'
as explained below.  You may edit tex/Makefile to make
corrections before you run 'make install' in this directory.\n";

    print "\n",
          "    MFDIR directory  : $v->{MFDIR}\n",
          "   TFMDIR directory  : $v->{TFMDIR}\n",
          "TEXINPUTS directory  : $v->{TEXINPUTS}\n",
          "  PKFONTS directories: ", join(",\n                       ",
                                        @{$v->{PKFONTS}}), "\n",
          ;
    print "
    MFDIR is where I will install the font files (tex/*.mf).
    TFMDIR is where I will install the font metric files
        (tex/*.tfm).
    TEXINPUTS is where I will install the TeX macro input files
        (tex/gooemacs.tex and tex/gotcmacs.tex).
    PKFONTS is where there might be old cached GOOE fonts (go*pk)
        that I will delete.\n\n";


    if (-f 'tex/Makefile') {    # rename original tex/Makefile
        unlink('tex/Makefile.old'); # in case there's already an old one
        rename('tex/Makefile', 'tex/Makefile.old') or
            die "Error renaming tex/Makefile to tex/Makefile.old: $!";
    } else {
        die "Oops! No tex/Makefile, you will have to install will by hand.\n";
    }
    my $texOldFd = IO::File->new("<tex/Makefile.old") or
        die "Can't open tex/Makefile.old for reading: $!";
    my $texNewFd = IO::File->new(">tex/Makefile") or
        die "Can't open tex/Makefile for writing: $!";

    my $time = localtime;
    my $install = $dos ? 'copy' : 'install -m 0644';
    my $newText = "
# $time: 'perl Makefile.PL' determined the following install variables:

# where font (*.mf) files will go:
MFDIR = $v->{MFDIR}

# where font metric (*.tfm) files will go:
TFMDIR = $v->{TFMDIR}

# where TeX input files (gooemacs.tex and gotcmacs.tex) will go:
TEXINPUTS = $v->{TEXINPUTS}

# where cached fonts (GOOE/go*pk) might have been put (we need to
# remove cached fonts from previous installs):
PKFONTS = $pkfonts

# how to install files:
INSTALL = $install

";
    my $autoEdit = 'not started';
    while(<$texOldFd>) {
        if ($autoEdit eq 'not started') {
            if (/^#\s*start perl Makefile.PL auto-edit\s*$/) {
                $autoEdit = 'started';
            }
        } elsif ($autoEdit eq 'started') {
            if(/^#\s*end perl Makefile.PL auto-edit\s*$/) {
                $texNewFd->print($newText);
                $texNewFd->print($_);
                $autoEdit = 'done';
            }
            next;
        }
        $texNewFd->print($_);
    }
    close $texOldFd;
    close $texNewFd;
    if ($autoEdit eq 'not started') {
        print "'# start perl Makefile auto-edit' section not found in tex/Makefile, reverting changes\n";
        unlink 'tex/Makefile';
        rename ('tex/Makefile.old', 'tex/Makefile') or
            die "Error renaming tex/Makefile.old to tex/Makefile: $!";
    } elsif ($autoEdit eq 'started') {
        print "'# end perl Makefile auto-edit' section not found in tex/Makefile, reverting changes\n";
        unlink 'tex/Makefile';
        rename ('tex/Makefile.old', 'tex/Makefile');
            die "Error renaming tex/Makefile.old to tex/Makefile: $!";
    }
}

# we need to adjust the Makefile created by ExtUtils::MakeMaker::WriteMakefile
sub editMakefile {

    unlink 'Makefile.tmp';
    rename ('Makefile', 'Makefile.tmp') or              # first, rename the original
        die "Error renaming Makefile to Makefile.tmp: $!";

    my $mfFd = IO::File->new("<Makefile.tmp") or        # open original Makefile for reading
        die "Can't open Makefile.tmp for reading: $!";

    my $new_mfFd = IO::File->new(">Makefile") or        # create a new Makefile for writing
        die "Can't open Makefile for writing: $!";

    # a dot-rule for creating .dvi from .tex files - may already be built-in
    #   to make, but hey, it doesn't hurt to make sure.  We'll add this rule
    #   to the MakeMaker c_o section
    my $tex2dvi_rule = '
.tex.dvi :
	tex $<
';

    while (<$mfFd>) {
        s/^(\.SUFFIXES.*?:)/$1 .tex .dvi/;              # add dvi and tex suffixes
        s/MakeMaker c_o section:.*/$1\n$tex2dvi_rule/;  # add rule for building .dvi from .tex
        s/^(install\s*:.*)/$1 install_tex manual.dvi/;  # add install_tex and manual.dvi targets to install
        $new_mfFd->print($_);
    }
    close $mfFd;
    close $new_mfFd;
    unlink 'Makefile.tmp';
}

sub getVariables {

    get_TEXMF();        # try to find the TEXMF root directory
    myFind($texmfRoot); # build database of directory names below TEXMF root
    myFind('/var');     # add directories in /var
    get_PKFONTS();
    get_TEXINPUTS();
    getMFDIR();
    get_TFMDIR();
    return { TEXMF => $texmfRoot,
             MFDIR => $mfdir,
             TFMDIR => $tfmdir,
             TEXINPUTS => $texInputs,
             PKFONTS => [keys(%pkFontHash)],
            };
}

sub get_TEXMF {

    print "try kpsewhich... ";
    `kpsewhich -expand-var \\\$TEXMF 2>/dev/null`; # try it
    $kpsewhich = WIFEXITED($?);     # normal exit?
    # $kpsewhich = 0;                 # prevent kpsewhich

    # try to find TEXMF root directory candidates
    my %tmfRootHash;
    kpsewhich_get(\%tmfRootHash, qw(TEXMF TEXMFLOCAL TEXMFMAIN VARTEXMF HOMETEXMF));
    locateRoot(\%tmfRootHash);      # try 'locate'
    foreach (keys(%tmfRootHash)) {
        delete($tmfRootHash{$_}) if (m#^/var/#);    # /var should contain only variable stuff
    }

    print "\n";
    unless(scalar(keys(%tmfRootHash))) {
        print "\nHmmm, I can't find your root TEXMF directory.  I'll have to ask for\n",
              " your help.  You may find the INSTALL file has some useful hints.\n\n";
        $tmfRootHash{enter_directory()} = 1;
    }

    if (scalar(keys(%tmfRootHash)) > 1) {
        print "Looks like the TEXMF root (install) directory is one of these:\n\n";
        $texmfRoot = selectDir(keys(%tmfRootHash));
    } else {
        foreach (keys(%tmfRootHash)) {
            $texmfRoot = $_;        # there's only one
        }
    }
}


sub get_PKFONTS {
    # collect possible PKFONTS directories
    kpsewhich_get(\%pkFontHash, 'PKFONTS');       # get kpsewhich's idea of PKFONTS
    foreach(grep m#$texmfRoot/.*\bGOOE$#, keys(%find)) {    # and do our own search as well
        $pkFontHash{$_} = 1;
    }
    foreach(grep m#/texmf/.*\bpk/.*\bGOOE$#, keys(%find)) {  # and outside the root
        $pkFontHash{$_} = 1;
    }
    # remove PKFONTS directories that don't contain go*pk files
    foreach (keys(%pkFontHash)) {
        my @globs = glob("$_/go*pk");
        delete ($pkFontHash{$_}) unless (@globs > 0);
    }
}

sub get_TEXINPUTS {

    # collect possible TEXINPUTS directories
    my %texInputHash;
    kpsewhich_get(\%texInputHash, 'TEXINPUTS');   # get kpsewhich's idea of TEXINPUTS
    foreach(grep m#$texmfRoot/tex$#, keys(%find)) {        # and do our own search as well
        $texInputHash{$_} = 1;
    }
    foreach(grep m#/tex/.*\bGOOE$#, keys(%find)) {        # and do our own search as well
        $texInputHash{$_} = 1;
    }
    # if there's already a GOOE TEXINPUT directory, remove all others
    my @texInputsArray;
    foreach (keys(%texInputHash)) {
        push(@texInputsArray, $_) if (m#/GOOE\b#);
    }

    # if there are no GOOEs already (first-time install), copy our collection
    @texInputsArray = keys(%texInputHash) unless (@texInputsArray);

    # if no TEXINPUT directories yet, we have to ask for input
    unless(@texInputsArray) {
        print "\nYour root TEXMF directory is $texmfRoot, but I can't find a TEXINPUTS\n",
              "  subdirectory under it.  Please see the INSTALL file for more information.\n",
              "\nPlease enter a directory for TEXINPUTS (it must already exist):\n\n";
        push(@texInputsArray, enter_directory());
    }

    # if multiple TEXINPUTS, ask user to select one
    if (@texInputsArray > 1) {
        print "Looks like the TEXINPUTS directory is one of these:\n\n";
        $texInputs = selectDir(@texInputsArray);
    } else {
        $texInputs = $texInputsArray[0];
    }
}

sub getMFDIR {

    # MFDIR is the path to the Metafont sources
    foreach ('fonts/source/public', 'fonts/source', '/metafont/misc') {
        if (-d "$texmfRoot/$_") {
            $mfdir = "$texmfRoot/$_";
            last;
        }
    }
    # if no mfdir directory yet, we have to ask for input
    unless(defined($mfdir)) {
        print "\nYour root TEXMF directory is $texmfRoot, but I can't find the right\n",
              "  place to put the font source (*.mf) files.\n",
              "\nPlease enter a directory for MFDIR, probably somewhere under $texmfRoot\n",
              "   (it must already exist, but I will add /GOOE to the end of what you enter):\n\n";
        $mfdir = enter_directory();
    }
    $mfdir =~ s#/GOOE$##;
    $mfdir .= '/GOOE';
}

sub get_TFMDIR {

    # TFMDIR is the path to the .tfm files.
    foreach ('fonts/tfm/public', 'fonts/tfm') {
        if (-d "$texmfRoot/$_") {
            $tfmdir = "$texmfRoot/$_";
            last;
        }
    }
    # if no mfdir directory yet, we have to ask for input
    unless(defined($tfmdir)) {
        print "\nYour root TEXMF directory is $texmfRoot, but I can't find the right\n",
              "  place to put the font metric (*.tfm) files.\n",
              "\nPlease enter a directory for TMFDIR, probably somewhere under $texmfRoot\n",
              "   (it must already exist, but I will add /GOOE to the end of what you enter):\n\n";
        $tfmdir = enter_directory();
    }
    $tfmdir =~ s#/GOOE$##;
    $tfmdir .= '/GOOE';
}

sub wanted {
    -d $_ &&    # could use magic _ here, but time diff is minimal and $_ is robust
    ($find{$_} = 1);
}

sub myFind {
    my ($root) = @_;

    no warnings;
    File::Find::find({wanted => \&wanted,
                      follow_fast => 1,
                      follow_skip => 2,
                      no_chdir => 1},
                      $root);
}

sub locateRoot {
    my $hash = shift;

    print "try locate...";
    my @locate = `locate texmf`;
    unless (WIFEXITED($?)) {
        # print "\$?=$?\n";
        return 0;
    }
    grep( { m#(.*/texmf)/# && ($hash->{$1} = 1); } @locate);  # save everything with '/texmf/'
    return 1;
}


sub kpsewhich_get {
    my $hash = shift;

    return unless($kpsewhich);
    foreach my $dir (@_) {
        my $str = `kpsewhich -expand-var \\\$$dir 2>/dev/null`;
        chomp $str;
        $str = `kpsewhich --expand-braces "$str"`;
        chomp $str;
        $str =~ s#//*#/#g;
        my @a = split(':', $str);
        foreach my $name (grep( {-d $_} grep( {not $_ eq '.'} @a))) {
            $name =~ s#/*$##;   # remove any trailing slashes
            $hash->{$name} = 1;
        }
    }
}

sub selectDir {
    my @dirs = @_;

    my ($ii, $rsp);
    do {
        for($ii = 1; $ii <= @dirs; $ii++) {
            print "    $ii. $dirs[$ii - 1]\n";
        }
        print "    $ii. Select a different directory\n";
        print "    q. Quit\n";
        $rsp = prompt("\nPlease select one: ", 'q');
        chomp $rsp;
        die ("Quitting...\n") if ((lc($rsp) eq 'q') or
                                  (lc($rsp) eq ''));
    } while(not defined($rsp) or
            ($rsp == 0) or
            ($rsp =~ m/\D/) or
            ($rsp > $ii));
    if ($rsp == $ii) {
        return(enter_directory());
    }
    return $dirs[$rsp - 1];
}

sub enter_directory {

    my $rsp;
    do {
        $rsp = prompt("\nPlease enter directory name (or 'q' to quit): ", 'q');
        chomp $rsp;
        die ("Quitting...\n") if ((lc($rsp) eq 'q') or
                                  (lc($rsp) eq ''));
        unless (-d $rsp) {
            print "\n$rsp is not a directory"
        }
    } while(not defined($rsp) or
            (not -d $rsp));
    return $rsp;
}


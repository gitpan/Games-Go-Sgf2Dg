
use 5.005;
use strict;
use ExtUtils::MakeMaker;
use IO::File;
use Config;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

our $osname = lc($Config{osname});
our $dos = (($osname eq 'dos') or ($osname eq 'win32'));

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

WriteMakefile(
    NAME                => 'Games::Go::Sgf2Dg', # package name
    VERSION_FROM        => 'sgf2dg',            # finds $VERSION
    EXE_FILES           => ['sgf2dg',  $dos ? 'sgfsplit.exe' : 'sgfsplit'],
    MAN1PODS            => {sgf2dg      => "\$(INST_MAN1DIR)/sgf2dg.1",
                            'sgfsplit.c' => "\$(INST_MAN1DIR)/sgfsplit.1",},
    ABSTRACT            => "sgf2dg (replaces sgf2tex) converts Smart Go Format (SGF) files to Go diagrams - includes the GOOE TeX fonts",
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (AUTHOR     => 'Dan Bump <bump@match.stanford.edu>, Reid Augustin <reid@hellosix.com>') : ()),
    dist => { COMPRESS=>"gzip", SUFFIX=>"gz" },
);

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

# no dynamic targets for this package:
sub MY::dynamic {
    return '';
}

sub editTexMakefile {
    # installing TeX stuff is a bit tricky - we don't know where it
    #   might be on this system.  try kpsewhich:

    die "Can't 'do ./findTEXMF: $!$@\n" unless(defined(do './findTEXMF'));
    my $v;
    eval { $v = getVariables(); };      # try to find TEXMF variables
    die ("\nUser Abort - TeX fonts and macros will not be installed during 'make install'.\n",
         "             However, you can still 'make install' for the rest of Sgf2Dg\n\n") if ($@);
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



#!/usr/bin/perl -w

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Games-Go-Dg2TeX.t'

#########################

use strict;
use IO::File;
use Test::More tests => 18;

BEGIN {
    use_ok('Games::Go::Dg2TeX');
    use_ok('Games::Go::Diagram');
};

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $dg2tex;

##
## create dg2tex object:
##
my ($tex1, $tex);        # collect TeX here
eval { $dg2tex = Games::Go::Dg2TeX->new(
        doubleDigits => 0,
        coords       => 1,
        file         => \$tex1); };
is( $@, '',                                     'new Dg2TeX object' );
isa_ok( $dg2tex, 'Games::Go::Dg2TeX',           '   dg2tex is the right class' );

eval {$dg2tex->comment(' comment')};
is( $@, '',                                     'added comment' );
is( $tex1, "% comment\n",                        '    tex is correct');
$dg2tex->comment(' and more comment');
is( $tex1, "% comment\n% and more comment\n",     'added more comment' );
eval { $dg2tex->configure(
        file => \$tex,); };
is( $@, '',                                     're-configure' );
eval {$dg2tex->print('raw print', "\n")};
is( $@, '',                                     'raw print' );
is( $tex, "\\magnification=1200
\\input gooemacs
\\gool
\\newdimen\\diagdim
\\newdimen\\fulldim
\\newbox\\diagbox
\\newbox\\captionbox
raw print
",                                              '    raw print is good' );
$tex = '';
is( $dg2tex->converted, "% comment
% and more comment
\\magnification=1200
\\input gooemacs
\\gool
\\newdimen\\diagdim
\\newdimen\\fulldim
\\newbox\\diagbox
\\newbox\\captionbox
raw print
",                                              'converted TeX is good' );
is( $dg2tex->converted(''), '',                 'converted TeX cleared' );
is( $dg2tex->TeXifyText('this <is> a {TeX} \conversion_test'), 
                        "this [is] a [TeX] /conversion-test\\hfil\\break\n",
                                                'text-to-TeX conversion');
$dg2tex->convertProperties({GN => ['GameName'],
                            EV => ['EVent'],
                            RO => ['ROund'],
                            PW => ['PlayerWhite'],
                            WR => ['WhiteRank'],
                            C  => ['PlayerBlack', 'is not here']});
is($dg2tex->converted, 
"{\\noindent
GameName\\hfil\\break
EVent - Round ROund\\hfil\\break
{\\bf White:} PlayerWhite WhiteRank\\hfil\\break
\\vfil}
\\nobreak
",                                              'convertProperties');

my $diagram;
eval { $diagram = Games::Go::Diagram->new(
                    hoshi             => ['ba', 'cd'],
                    black             => ['ab'],
                    white             => ['dd', 'cd'],
                    callback          => \&conflictCallback,
                    enable_overstones => 1,
                    overstone_eq_mark => 1); };
die "Can't create diagram: $@" if $@;

eval { $dg2tex->configure( boardSize => 5,); };
is( $@, '',                                     'reconfigured Dg2TeX object' );
is( $dg2tex->converted(''), '',                 'converted TeX cleared' );
eval { $dg2tex->convertDiagram( $diagram); };
is( $@, '',                                     'converted Diagram' );
is ($dg2tex->converted,
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Start of Unknown Diagram
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setbox\diagbox=\vbox to 87.0 pt{\hsize= 75.0 pt\goo
\0??<\0??*\0??(\0??(\0??>\raise 3pt\hbox to 15pt{\hglue5pt\rm 5\hfil}
\- @[\0??+\0??+\0??+\0??]\raise 3pt\hbox to 15pt{\hglue5pt\rm 4\hfil}
\0??[\0??+\0??+\0??+\0??]\raise 3pt\hbox to 15pt{\hglue5pt\rm 3\hfil}
\0??[\0??+\- !+\- !+\0??]\raise 3pt\hbox to 15pt{\hglue5pt\rm 2\hfil}
\0??,\0??)\0??)\0??)\0??.\raise 3pt\hbox to 15pt{\hglue5pt\rm 1\hfil}
\vfil
\hbox{\hglue3pt\vbox{\hsize=60 pt\settabs5\columns\rm
\+A&B&C&D&E\cr}\hfil}
\vglue5pt\vfil}
\setbox\captionbox=\vbox{\tolerance=10000\vglue-8pt
\parindent=0pt\parskip=8pt\vglue6pt\lineskip=0pt\baselineskip=12pt
\hangindent 95 pt\hangafter-7
\noindent{\bf Unknown Diagram}\hfil\break\hfil\break
\hfil\break
\par\vfil}
\diagdim=\ht\diagbox
\ifdim\ht\captionbox>\diagdim\fulldim=\ht\captionbox
  \else\fulldim=\diagdim\fi
\vbox to\fulldim{\box\diagbox\vglue-\diagdim\box\captionbox}

' ,                                             '    TeX is correct' );

##
## end of tests
##

__END__

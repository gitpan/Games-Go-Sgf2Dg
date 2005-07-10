#!/usr/bin/perl -w

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Games-Go-Dg2ASCII.t'

#########################

use strict;
use IO::File;
use Test::More tests => 17;

BEGIN {
    use_ok('Games::Go::Dg2ASCII');
    use_ok('Games::Go::Diagram');
};

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $dg2ascii;

##
## create dg2ascii object:
##
my ($text1, $text);        # collect text here
eval { $dg2ascii = Games::Go::Dg2ASCII->new(
        doubleDigits => 0,
        coords       => 1,
        file         => \$text1); };
is( $@, '',                                     'new Dg2ASCII object'  );
isa_ok( $dg2ascii, 'Games::Go::Dg2ASCII',       '   dg2ascii is the right class'  );

eval {$dg2ascii->comment(' comment')};
is( $@, '',                                     'added comment' );
is( $text1, " comment\n",                       '    text is correct');
$dg2ascii->comment(' and more comment');
is( $text1, " comment\n and more comment\n",    'added more comment' );
eval { $dg2ascii->configure(
        file => \$text,); };
is( $@, '',                                     're-configure' );
eval {$dg2ascii->print('raw print', "\n")};
is( $@, '',                                     'raw print' );
is( $text, "raw print
",                                              '    raw print is good' );
$text = '';
is( $dg2ascii->converted, " comment
 and more comment
raw print
",                                              'converted text is good' );
is( $dg2ascii->converted(''), '',               'converted text cleared' );
$dg2ascii->convertProperties({GN => ['GameName'],
                            EV => ['EVent'],
                            RO => ['ROund'],
                            PW => ['PlayerWhite'],
                            WR => ['WhiteRank'],
                            C  => ['PlayerBlack', 'is not here']});
is($dg2ascii->converted, 
"
GameName
EVent - Round ROund
White: PlayerWhite WhiteRank

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

eval { $dg2ascii->configure( boardSize => 5,); };
is( $@, '',                                     'reconfigured Dg2ASCII object' );
is( $dg2ascii->converted(''), '',               'converted text cleared' );
eval { $dg2ascii->convertDiagram( $diagram); };
is( $@, '',                                     'converted Diagram' );
is ($dg2ascii->converted,
'
Black -> X   Marked black -> #   Labeled black -> Xa, Xb
White -> O   Marked white -> @   Labeled white -> Oa, Ob
Unknown Diagram
 +-- *  ---------+  5
 |               |  
 X   +   +   +   |  4
 |               |  
 |   +   +   +   |  3
 |               |  
 |   +   O   O   |  2
 |               |  
 +---------------+  1
 A   B   C   D   E



',                                             '    text is correct' );

##
## end of tests
##

__END__

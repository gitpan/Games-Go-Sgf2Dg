# $Id: Dg2TeX.pm 143 2005-06-03 21:05:57Z reid $

#   Dg2TeX
#
#   Copyright (C) 2005 Reid Augustin reid@hellosix.com
#                      1000 San Mateo Dr.
#                      Menlo Park, CA 94025 USA
#
#   This library is free software; you can redistribute it and/or modify it
#   under the same terms as Perl itself, either Perl version 5.8.5 or, at your
#   option, any later version of Perl 5 you may have available.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE.
#

=head1 NAME

Games::Go::Dg2TeX - Perl extension to convert Games::Go::Diagrams to TeX

=head1 SYNOPSIS

use Games::Go::Dg2TeX

 my $dg2tex = B<Games::Go::Dg2TeX-E<gt>new> (options);
 my $tex = $gd2tex->convertDiagram($diagram);

=head1 DESCRIPTION

A Games::Go::Dg2TeX object converts a L<Games::Go::Diagram> object
into TeX source code which can be used stand-alone, or it can be
incorporated into larger TeX documents.

=cut

use strict;
require 5.001;

package Games::Go::Dg2TeX;
use Carp;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use PackageName ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
);

BEGIN {
    our $VERSION = sprintf "1.%03d", '$Revision: 143 $' =~ /(\d+)/;
}

######################################################
#
#       Class Variables
#
#####################################################

use constant TOPLEFT     => '<';
use constant TOPRIGHT    => ',';
use constant TOP         => '[';
use constant BOTTOMLEFT  => '>';
use constant BOTTOMRIGHT => '.';
use constant BOTTOM      => ']';
use constant LEFT        => '(';
use constant RIGHT       => ')';
use constant MIDDLE      => '+';
use constant HOSHI       => '*';
use constant EMPTY       => "\\0??";    # empty intersection
use constant WHITE       => "\\- !";    # numberless white stone
use constant BLACK       => "\\- @";    # numberless black stone
use constant MARKEDWHITE => "\\- ;";    # marked white stone
use constant MARKEDBLACK => "\\- :";    # marked black stone

use constant NORMAL_MACROS =>
"\\magnification=1200
\\input gooemacs
\\gool
\\newdimen\\diagdim
\\newdimen\\fulldim
\\newbox\\diagbox
\\newbox\\captionbox\n";

use constant SIMPLE_MACROS =>
"\\magnification=1200
\\input gooemacs
\\raggedbottom
\\parindent=0pt\n";

use constant TWO_COLUMN_MACROS =>
"\\magnification=1200
\\input gotcmacs
\\raggedbottom
\\tolerance=10000
\\parindent=0pt\n";

our %options = (
    boardSize       => 19,
    doubleDigits    => 0,
    topLine         => 1,
    bottomLine      => 19,
    leftLine        => 1,
    rightLine       => 19,
    diaCoords       => sub { my ($x, $y) = @_;
                             $x = chr($x - 1 + ord('a'));
                             $y = chr($y - 1 + ord('a'));
                             return("$x$y"); },
    file            => undef,
    filename        => 'unknown',
    print           => sub { return; }, # Hmph...
    longComments    => 0,
    simple          => 0,
    twoColumn       => 0,
    coords          => 0,
    bigFonts        => 0,
    texComments     => 0,
    gap             => 12,
    );

######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over 4

=item my $dg2tex = B<Games::Go::Dg2TeX-E<gt>new> (?options?)

A B<new> Games::Go::D2TeX takes the following options:

=head2 General Dg2 Converter Options:

=over 4

=item B<boardSize> =E<gt> number

Sets the size of the board.

Default: 19

=item B<doubleDigits> =E<gt> true | false

Numbers on stones are wrapped back to 1 after they reach 100.
Numbers associated with comments and diagram titles are not
affected.

Default: false

=item B<topLine>     =E<gt> number (Default: 1)

=item B<bottomLine>  =E<gt> number (Default: 19)

=item B<leftLine>    =E<gt> number (Default: 1)

=item B<rightLine>   =E<gt> number (Default: 19)

The edges of the board that should be displayed.  Any portion of the
board that extends beyond these numbers is not included in the
output.

=item B<diaCoords> =E<gt> sub { # convert $x, $y to Games::Go::Diagram
coordinates }

This callback defines a subroutine to convert coordinates from $x,
$y to whatever coordinates are used in the Games::Go::Diagram
object.  The default B<diaCoords> converts 1-based $x, $y to the
same coordinates used in SGF format files.  You only need to define
this if you're using a different coordinate system in the Diagram.

Default:
    sub { my ($x, $y) = @_;
          $x = chr($x - 1 + ord('a')); # convert 1 to 'a', etc
          $y = chr($y - 1 + ord('a'));
          return("$x$y"); },           # concatenate two letters

=item B<file> =E<gt> 'filename' | $descriptor | \$string | \@array

If B<file> is defined, the TeX source is dumped into the target.
The target can be any of:

=over 4

=item filename

The filename will be opened using IO::File->new.  The filename
should include the '>' or '>>' operator as described in 'perldoc
IO::File'.  TeX source is written into the file.

=item descriptor

A file descriptor as returned by IO::File->new, or a \*FILE
descriptor.  TeX source is written into the file.

=item reference to a string scalar

TeX source is concatenated to the end of the string.

=item reference to an array

TeX source is split on "\n" and each line is pushed onto the array.

=back

Default: undef

=item B<print> =E<gt> sub { my ($dg2tex, @tex) = @_; ... }

A user defined subroutine to replace the default printing method.
This callback is called from the B<print> method (below) with the
reference to the B<Dg2TeX> object and a list of lines that are
part of the TeX diagram source.

=back

=head2 Dg2TeX-specific options

=over 4

=item B<longComments> =E<gt> true | false

In its default usage, the comments to each diagram comprise an unbreakable
vbox---they must all appear on one page. This can cause problems if the
comments are very extensive. This option generates more complicated TeX macros
which allow the comments to be broken across pages. This option may not
be used with B<-simple> or B<-longComments>.

Default: true

=item B<simple> =E<gt> true | false

This generates very simple TeX which may not look so good on the page,
but is convenient if you intend to edit the TeX. This option should
not be used with B<-longComments>.

Default: false

=item B<twoColumn> =E<gt> true | false

This generates a two-column format using smaller fonts. This
option forces B<simple> true, and it may not be used with
B<-longComments> or B<-coords>.

Default: false

=item B<coords> =E<gt> true | false

Generates a coordinate grid. This option may not be used with B<-twoColumn>.

Default: false

=item B<bigFonts> =E<gt> true | false

Use fonts magnified 1.2 times.

Default: false

=item B<texComments> =E<gt> true | false

If this option is NOT used then the characters {, } and \ found in comments
are replaced by [, ] and /, since TeX roman fonts do not have these
characters. If this option is used, these substitutions are not made, so you
can embed TeX source (like {\bf change fonts}) directly inside the comments.

=item B<gap> =E<gt> number of points

The B<gap> in points between diagrams.

Default: 12

=back

=head2 Interactions between options

If B<coords> and B<twoColumn> are both true, Dg2TeX warns and
turns off B<coords>.  If B<longComments> and B<simple> are both
true, warns and turns off B<longComments>.  If B<longComments>
and B<twoColumn> are both true, warns and turns off
B<longComments>.  Finally, if B<twoColumn> is true, turns
B<simple> on (no warning).

=cut

sub new {
    my ($proto, %args) = @_;

    my $my = {};
    bless($my, ref($proto) || $proto);
    $my->{converted} = '';
    foreach (keys(%options)) {
        $my->{$_} = $options{$_};  # transfer default options
    }
    # transfer user args
    $my->configure(%args);
    return($my);
}

=item $dg2tex-E<gt>B<configure> (option =E<gt> value, ?...?)

Change Dg2TeX options from values passed at B<new> time.

=cut

sub configure {
    my ($my, %args) = @_;

    if (exists($args{file})) {
        $my->{file} = delete($args{file});
        if (ref($my->{file}) eq 'SCALAR') {
            $my->{filename} = $my->{file};
            $my->{print} = sub { ${$_[0]->{file}} .= $_[1]; };
        } elsif (ref($my->{file}) eq 'ARRAY') {
            $my->{filename} = 'ARRAY';
            $my->{print} = sub { push @{$_[0]->{file}}, split("\n", $_[1]); };
        } elsif (ref($my->{file}) eq 'GLOB') {
            $my->{filename} = 'GLOB';
            $my->{print} = sub { $_[0]->{file}->print($_[1]) or
                                        die "Error writing to output file:$!\n"; };
        } elsif (ref($my->{file}) =~ m/^IO::/) {
            $my->{filename} = 'IO';
            $my->{print} = sub { $_[0]->{file}->print($_[1]) or
                                        die "Error writing to output file:$!\n"; };
        } else {
            require IO::File;
            $my->{filename} = $my->{file};
            $my->{file} = IO::File->new($my->{filename}) or
                die("Error opening $my->{filename}: $!\n");
            $my->{print} = sub { $_[0]->{file}->print($_[1]) or
                                        die "Error writing to $_[0]->{filename}:$!\n"; };
        }
    }
    foreach (keys(%args)) {
        croak("I don't understand option $_\n") unless (exists($options{$_}));
        $my->{$_} = $args{$_};  # transfer user option
    }
    if ($my->{coords} and
        $my->{twoColumn}) {
        carp("\nWarning: -coords and -twoColumn cannot be used together - turning off coords.");
        delete($my->{coords});
    }
    if ($my->{longComments} and
        $my->{simple}) {
        carp("\nWarning: -longComments and -simple cannot be used together - turning off longComments.");
        delete($my->{longComments});
    }
    if ($my->{longComments} and
        $my->{twoColumn}) {
        carp("\nWarning: -longComments and -twoColumn cannot be used together - turning off -longComments.");
        delete($my->{longComments});
    }
    if ($my->{twoColumn}) {
        $my->{simple} = 1;
    }
    $my->{fontSize} = ($my->{twoColumn}) ? 10 : 12;
    # make sure edges of the board don't exceed boardSize
    $my->{topLine}    = 1 if ($my->{topLine} < 1);
    $my->{leftLine}   = 1 if ($my->{leftLine} < 1);
    $my->{bottomLine} = $my->{boardSize} if ($my->{bottomLine} > $my->{boardSize});
    $my->{rightLine}  = $my->{boardSize} if ($my->{rightLine} > $my->{boardSize});
}

=item $dg2tex-E<gt>B<print> ($tex ? , ... ?)

B<print>s raw TeX code to B<file> as defined at B<new> time.
Whether or not B<file> was defined, B<print> accumulates the TeX
code for later retrieval with B<converted>.

=cut

sub print {
    my ($my, @args) = @_;

    # one-time init:
    unless(exists($my->{macrosDone})) {
        $my->{macrosDone} = 1;
        if (not $my->{simple}) {
            $my->print(NORMAL_MACROS);
        } elsif ($my->{twoColumn}) {
            $my->print(TWO_COLUMN_MACROS); 
        } else {
            $my->print(SIMPLE_MACROS);
        }
    }
    foreach my $arg (@args) {
        $my->{converted} .= $arg;
        &{$my->{print}} ($my, $arg);
    }
}

=item my $tex = $dg2tex-E<gt>B<converted> ($replacement_tex)

Returns the TeX source code converted so far for the B<Dg2TeX>
object.  If $replacement_tex is defined, the accumulated TeX source
code is replaced by $replacement_tex.

=cut

sub converted {
    my ($my, $tex) = @_;

    $my->{converted} = $tex if (defined($tex));
    return ($my->{converted});
}

=item $dg2tex-E<gt>B<comment> ($comment ? , ... ?)

Inserts the TeX comment character ('%') in front of each line of
each comment and B<print>s it to B<file>.

=cut

sub comment {
    my ($my, @comments) = @_;

    local $my->{macrosDone} = 1;        # allow comments before one-time init
    foreach my $c (@comments) {
        while ($c =~ s/([^\n]*)\n//) {
            $my->print("%$1\n");
        }
        $my->print("%$c\n") if ($c ne '');
    }
}

=item my $tex_source = $dg2tex-E<gt>B<convertDiagram> ($diagram)

Converts a I<Games::Go::Diagram> into TeX.  If B<file> was defined
in the B<new> method, the TeX source is dumped into the B<file>.
In any case, the TeX source is returned as a string scalar.

=cut

sub convertDiagram {
    my ($my, $diagram) = @_;

    $my->convertProperties($diagram->property(0));      # any game-level properties?
    my @name = $diagram->name;
    $name[0] = 'Unknown Diagram' unless(defined($name[0]));
    my $propRef = $diagram->property;                   # get property list for the diagram
    my $first = $diagram->first_number;
    my $last = $diagram->last_number;
    $my->{offset} = $diagram->offset;
    $my->{stoneOffset} = $diagram->offset;
    if ($my->{doubleDigits}) {
        while ($first - $my->{stoneOffset} >= 100) {
            $my->{stoneOffset} += 100;      # first to last is not supposed to cross 101
        }
    }
    my $range = '';
    if ($first) {
        $range = ': ' . ($first - $my->{offset});
        if ($last != $first) {
            $range .= '-' . ($last - $my->{offset});
        }
    } else {
        # carp("Hmmm! No numbered moves in $name[0]");
    }
    $my->print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    $my->print("%  Start of ", @name, "$range\n");
    $my->print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");

    if (exists($propRef->{0}{N})) {
        $range .= "\n\n$propRef->{0}{N}";       # node name
    }
    # get some measurements based on font size
    my ($h, $w) = (($my->{bottomLine} - $my->{topLine} + 1), ($my->{rightLine} - $my->{leftLine} + 1));
    my $diaHeight = (1 + .2 * $my->{bigFonts}) * ($my->{fontSize} * $h + $my->{gap});
    my $diaWidth = $my->{fontSize} * (1 + .2 * $my->{bigFonts}) * $w;
    if ($my->{coords}) {
        $diaWidth += 15;
        $diaHeight += 15;
    }
    # figure out whether we need odd or even parity for this diagram
    delete($my->{goFont});
    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        foreach my $x ($my->{leftLine} ..  $my->{rightLine}) {
            my $int = $diagram->get(&{$my->{diaCoords}}($x, $y));
            if (exists($int->{number})) {
                $my->_intersectionFont($int);   # to set goFont
            }
            last if (exists($my->{goFont}));
        }
        last if (exists($my->{goFont}));
    }
    $my->{goFont} = 'goo' unless (exists($my->{goFont}));
    $my->_preamble($diaHeight, $diaWidth);
    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        foreach my $x ($my->{leftLine} ..  $my->{rightLine}) {
            $my->_TeXifyIntersection($diagram, $x, $y);
        }
        if ($my->{coords}) {    # right-side coords
            $my->print("\\raise 3pt\\hbox to 15pt{\\hglue5pt\\rm ", $my->{boardSize} - $y + 1, "\\hfil}");
        }
        $my->print("\n");
    }
    # print the coordinates and diagram title
    $name[0] = "{\\bf $name[0]}";      # boldface the first name line
    if (defined($diagram->var_on_move) and
        defined($diagram->parent)) {
        my $varOnMove = $diagram->var_on_move;
        my $parentOffset = $diagram->parent->offset;
        my $parentName = $diagram->parent->name->[0];
        if (defined($parentOffset) and
            defined($parentName)) {
            $name[0] .= ' at move ' .
                        ($varOnMove - $parentOffset) .
                        ' in ' .
                        $parentName;
        }
    }
    $my->_interlude(join('', @name, $range), $diaWidth, $diaHeight);

    # deal with the over-lay stones
    $my->_TeXifyOverstones($diagram);
    if ($my->{twoColumn}) {
        $my->print("}\n");
    } else { 
        $my->print("\\hfil\\break\n");
    }
    # print the game comments for this diagram
    foreach my $n (sort { $a <=> $b } keys(%{$propRef})) {
        my @comment;
        if ((exists($propRef->{$n}{B}) and
             ($propRef->{$n}{B}[0] eq 'tt')) or
            (exists($propRef->{$n}{W}) and
             ($propRef->{$n}{W}[0] eq 'tt'))) {
            push(@comment, "Pass\n\n");
        }
        if (exists($propRef->{$n}{C})) {
            push(@comment, @{$propRef->{$n}{C}});
        }
        if (@comment) {
            my $c = '';
            my $n_off = $n - $my->{offset};
            $c = "$n_off: " if (($n > 0) and
                                ($n >= $first) and
                                ($n <= $last));
            $c .= join('', @comment);
            $my->print($my->TeXifyText($c));
        }
    }
    $my->_postamble();
}

=item my $tex = $dg2tex-E<gt>B<TeXifyText> ($text)

Converts $text into TeX code by changing certain characters that are
not available in TeX cmr10 font, and by converting \n\n into
\hfil\break.  B<TeXifyText> behavior is modified by B<texComments>
and B<simple> options.

Returns the converted text.

=cut

sub TeXifyText {
    my ($my, $text) = @_;

    if ($my->{texComments}) {
        $text =~ tr/<>_/[]-/            # \{} are untouched if texComments is true
    } else {
        $text =~ s/\\/\//gs;            #  \\ -> / since cmr10 has no backslash
        $text =~ tr/{<>}_/[[]]-/;       #  cmr10 has no {<>}_ so substitute [[]]-
    }
    $text =~ s/([&~^\$%#])/\\$1/gs;     #  escape &~^$%#

    if ($my->{simple}) {
        $text .= "\n\n";                # just tack two newlines to the end
    } else {
        $text =~ s/\n(\s*\n)+/\n\\hfil\\break\n/gs;      # replace \n\n by \hfil\break
        $text .= "\\hfil\\break\n"
    }
    return($text);
}

=item $tex_title = $dg2tex-E<gt>B<convertProperties> (\%sgfHash)

B<convertProperties> takes a reference to a hash of properties as
extracted from an SGF file.  Each hash key is a property ID and the
hash value is a reference to an array of property values:
$hash->{propertyId}->[values].  The following SGF properties are
recognized:

=over 4

=item GN GameName

=item EV EVent

=item RO ROund

=item PW PlayerWhite

=item WR WhiteRank

=item PB PlayerBlack

=item BR BlackRank

=item DT DaTe

=item PC PlaCe

=item GC GameComment

=item KM KoMi

=item RE REsult

=item TM TiMe

=back

Both long and short property names are recognized, and all
unrecognized properties are ignored with no warnings.  Note that
these properties are all intended as game-level notations.

=cut

sub convertProperties {
    my ($my, $hashRef) = @_;

    return unless(defined($hashRef));
    my %hash;
    foreach my $key (keys(%{$hashRef})) {
        my $short = $key;
        $short =~ s/[^A-Z]//g;                  # delete everything but upper case letters
        $hash{$short} = join('', @{$hashRef->{$key}});
    }

    my @lines;
    push(@lines, $hash{GN}) if(exists($hash{GN}));      # GameName
    if (defined($hash{EV})) {
        if (defined($hash{RO})) {
            push(@lines, "$hash{EV} - Round $hash{RO}");# EVent name and ROund number
        } else {
            push(@lines, $hash{EV});                    # EVent
        }
    }
    if (defined($hash{PW})) {
        if(defined($hash{WR})) {
            push(@lines, "{\\bf White:} $hash{PW} $hash{WR}");  # PlayerWhite and WhiteRank
        } else {
            push(@lines, "{\\bf White:} $hash{PW}");            # PlayerWhite
        }
    }
    if (defined($hash{PB})) {
        if(defined($hash{BR})) {
            push(@lines, "{\\bf Black:} $hash{PB} $hash{BR}");  # PlayerBlack and BlackRank
        } else {
            push(@lines, "{\\bf Black:} $hash{PB}");            # PlayerBlack
        }
    }
    push(@lines, $hash{DT}) if (defined($hash{DT}));            # DaTe
    push(@lines, $hash{PC}) if (defined($hash{PC}));            # PlaCe
    push(@lines, $hash{GC}) if (defined($hash{GC}));            # GameComment
    if (defined($hash{KM})) {                                   # komi
        if ($hash{KM} =~ m/(\d+\.\d+?)0*$/) {
            # remove ugly trailing zeros supplied by IGS
            $hash{KM} = $1;
        }
        push(@lines, "{\\bf Komi}: $hash{KM}");
    }
    push(@lines, "{\\bf Result}: $hash{RE}") if (defined($hash{RE}));   # result
    push(@lines, "{\\bf Time}: $hash{TM}") if (defined($hash{TM}));     # time constraints
    my ($title)='';
    foreach my $line (@lines) {
        next unless (defined($line));
        $line =~ s/\\([][)(\\])/$1/g;                           # change escaped chars to non-escaped
        $title .= "$line\\hfil\\break\n";
    }
    if($title ne '') {
        $my->print("{\\noindent\n$title\\vfil}\n\\nobreak\n");
    }
}

=item $dg2tex-E<gt>B<close>

B<print> the TeX closer (\bye) and close the dg2tex object.  Also
closes B<file> if appropriate.

=cut

sub close {
    my ($my) = @_;

    $my->print("\\bye\n");
    if (defined($my->{file}) and
        ((ref($my->{file}) eq 'GLOB') or
         (ref($my->{file}) eq 'IO::File'))) {
        $my->{file}->close;
    }
}

######################################################
#
#       Private methods
#
#####################################################

sub _TeXifyOverstones {
    my ($my, $diagram) = @_;

    my @tex;
    my $textFont = $my->{bigFont} ? "\\bigtextstone" : " \\textstone";     # choose font

    foreach my $int (@{$diagram->getoverlist()}) {
        my $overStones = '';
        for(my $ii = 0; $ii < @{$int->{overstones}}; $ii += 2) {
            # all the overstones that were put on this understone:
            my $overColor = $int->{overstones}[$ii];
            my $overNumber = $int->{overstones}[$ii+1];
            $overStones .= "\}, " if ($overStones ne '');
            local $my->{stoneOffset} = $my->{offset};
            $my->_stoneFont($overColor, $overNumber);   # make sure font is right
            $overStones .= sprintf("$textFont\{\\$my->{goFont}\\%03d=", $my->_checkStoneNumber($overNumber));
        }
        my $atStone = '';
        if (exists($int->{number})) {
            # numbered stone in text
            $atStone = $my->_intersectionFont($int) . sprintf("\\%03d=", $my->_checkStoneNumber($int->{number}));
        } else {
            unless (exists($int->{mark})) {
                my $mv = '';
                $mv .= " black node=$int->{black}" if (exists($int->{black}));
                $mv .= " white node=$int->{white}" if (exists($int->{black}));
                carp("Oops: understone$mv is not numbered or marked? " .
                     "This isn't supposed to be possible!");
            }
            if (exists($int->{black})) {
                $atStone = "\\- ::";            # marked black stone in text
            }elsif (exists($int->{white})) {
                $atStone = "\\- ;;";            # marked white stone in text
            } else {
                carp("Oops: understone is not black or white? " .
                     "This isn't supposed to be possible!");
            }
        }
        $atStone = "$textFont\{\\$my->{goFont}$atStone\}";
        # collect all the overstones in the diagram
        push(@tex, "$overStones\} at $atStone");
    }
    return '' unless(@tex);
    $my->print(join(",\\hfil\\break\\break\n", @tex) . "\\hfil\\break\\break\n");
}

sub _checkStoneNumber {
    my ($my, $number) = @_;

    if ($number - $my->{stoneOffset} > 0) {
        return $number - $my->{stoneOffset};
    }
    if ($number < 1) {
        carp "Yikes: stone number $number is less than 1.  Intersection/stone will be missing!";
    } else {
        carp "Stone number $number and offset $my->{stoneOffset} makes less than 1 - not using offset";
    }
    return $number;
}

# get tex for intersection hash from $diagram.
sub _TeXifyIntersection {
    my ($my, $diagram, $x, $y) = @_;

    my $int = $diagram->get(&{$my->{diaCoords}}($x, $y));
    my $stone;
    if (exists($int->{number})) {
        $stone = $my->_intersectionFont($int) . sprintf("\\%03d", $my->_checkStoneNumber($int->{number})); # numbered stone
    } elsif (exists($int->{mark})) {
        if (exists($int->{black})) {
            $stone = MARKEDBLACK;
        }elsif (exists($int->{white})) {
            $stone = MARKEDWHITE;
        } else {
            carp("Can't mark empty intersction");
        }
    } elsif (exists($int->{label})) {
        my $label = ((ord(lc($int->{label})) - ord('a')) % 26) + 401;
        if (exists($int->{black})) {
            $stone = sprintf("\\%03d", $label);         # black labels are 401 to 426
        } elsif (exists($int->{white})) {
            $stone = sprintf("\\%03d", $label + 100);   # white labels are 501 to 526
        } else {
            $my->print("\\!  $int->{label}");           # no underneath for labeled intersections
            return;
        }
    } elsif (exists($int->{white})) {
        $stone = WHITE;
    } elsif (exists($int->{black})) {
        $stone = BLACK;
    }

    if (defined($x) and defined($y)) {
        if (defined($stone)) {
            $stone .= $my->_underneath($x, $y);
        } else {
            $stone = EMPTY;       # empty intersection
            if (exists($int->{hoshi})) {
                $stone .= HOSHI;
            } else {
                $stone .= $my->_underneath($x, $y);
            }
        }
    }
    $my->print($stone);
}

# return the appropriate font char for the intersection
sub _underneath {
    my ($my, $x, $y) = @_;

    if ($x <= 1) {
        return TOPLEFT if ($y <= 1);            # upper left corner
        return TOPRIGHT if ($y >= $my->{boardSize}); # upper right corner
        return TOP;                             # upper side
    } elsif ($x >= $my->{boardSize}) {
        return BOTTOMLEFT if ($y <= 1);         # lower left corner
        return BOTTOMRIGHT if ($y >= $my->{boardSize}); # lower right corner
        return BOTTOM;                          # lower side
    }
    return LEFT if ($y <= 1);                   # left side
    return RIGHT if ($y >= $my->{boardSize});   # right side
    return MIDDLE;                              # somewhere in the middle
}

sub _intersectionFont {
    my ($my, $int) = @_;

    my $parity;
    my $color;

    if (exists($int->{black})) {
        $color = 'black';
    }
    if (exists($int->{white})) {
        if (defined($color)) {
            carp "intersction has both white and black stones!\n";
        }
        $color = 'white';
    }
    unless(defined($color)) {
        carp("can't set font for intersection with no stone");
        return '';
    }
    unless(exists($int->{number})) {
        carp("can't set font for intersection with un-numbered stone");
        return '';
    }
    return $my->_stoneFont($color, $int->{number});
}

sub _stoneFont {
    my ($my, $color, $number) = @_;

    my $parity = ($color eq 'black') ^ (($number - $my->{stoneOffset}) & 1);
    my $font = $parity ? 'goe' : 'goo';         # choose font based on color and odd/even number
    return('') if (exists($my->{goFont}) and ($font eq $my->{goFont}));
    $my->{goFont} = $font;
    return("\\$font");
}

sub _preamble {
    my ($my, $diaHeight, $diaWidth) = @_;

    my $b = $my->{bigFonts} ? 'b' : ''; # 'b' modifer for bigFonts
    my $vbox = sprintf("\\vbox to %3.1f pt{\\hsize= %3.1f pt", $diaHeight, $diaWidth);
    if ($my->{twoColumn}) {
        $my->print(           "\\vbox{$vbox\\$my->{goFont}\n");
    } elsif ($my->{simple}) {
        $my->print(                  "$vbox\\$b$my->{goFont}\n");
    } else {
        $my->print("\\setbox\\diagbox=$vbox\\$b$my->{goFont}\n");
    }
}

sub _interlude {
    my ($my, $title, $diaWidth, $diaHeight) = @_;

    # print coordinates along the bottom
    if ($my->{coords}) {
        my ($l, $r) = ($my->{leftLine}, $my->{rightLine});
        $my->print("\\vfil\n\\hbox{\\hglue3pt\\vbox{\\hsize=",
                12 * ($r - $l + 1) * (1 + .2*$my->{bigFonts}),
                " pt\\settabs",
                $r - $l + 1,
                "\\columns\\rm\n\\+",
                substr("A&B&C&D&E&F&G&H&J&K&L&M&N&O&P&Q&R&S&T&U&V&W&X&Y&Z", 2 * ($l - 1),
                                                                2 * ((($r <= 25) ? $r : 25) - $l) + 1),
                "\\cr}\\hfil}\n\\vglue5pt");
    }
    # print the diagram title
    $diaWidth += 20;
    if (($my->{twoColumn})or ($my->{simple})) {
        $my->print("}\n$title\\hfil\\break\n");
    } else {
        my $hangIndentLines = int(1 + $my->{bigFonts} + ($diaHeight - (1+.2*$my->{bigFonts})*$my->{gap})/ $my->{fontSize});
        $my->print("\\vfil}\n",
                "\\setbox\\captionbox=\\vbox{\\tolerance=10000\\vglue-8pt\n",
                "\\parindent=0pt\\parskip=8pt\\vglue6pt\\lineskip=0pt\\baselineskip=12pt\n",
                "\\hangindent $diaWidth pt\\hangafter-$hangIndentLines\n",
                "\\noindent$title\\hfil\\break\\hfil\\break\n");
    }
}

sub _postamble {
    my ($my) = @_;

    if ($my->{twoColumn}) {
        $my->print("\n\n");
    } elsif ($my->{longComments}) {
        $my->print("\\par\\vfil}\n",
                "\\diagdim=\\ht\\diagbox\n",
                "\\ifdim\\ht\\captionbox>280pt\n",
                "\\vbox to 280pt{\\box\\diagbox\\vglue-\\diagdim\\vsplit\\captionbox to 280pt}\n",
                "\\nointerlineskip\\unvbox\\captionbox\n",
                "\\else\n",
                "\\ifdim\\ht\\captionbox>\\diagdim\\fulldim=\\ht\\captionbox\n",
                "  \\else\\fulldim=\\diagdim\\fi\n",
                "\\vbox to\\fulldim{\\box\\diagbox\\vglue-\\diagdim\\box\\captionbox}\n",
                "\\fi\n\n");
    } elsif ($my->{simple}) {
        $my->print("\n\n");
    } else {
        # not LongComments and not Simple
        $my->print("\\par\\vfil}\n",
                "\\diagdim=\\ht\\diagbox\n",
                "\\ifdim\\ht\\captionbox>\\diagdim\\fulldim=\\ht\\captionbox\n",
                "  \\else\\fulldim=\\diagdim\\fi\n",
                "\\vbox to\\fulldim{\\box\\diagbox\\vglue-\\diagdim\\box\\captionbox}\n\n");
    }
}

1;

__END__

=back

=head1 SEE ALSO

=over 0

=item L<sgf2dg>(1)

Script to convert SGF format files to Go diagrams

=back

=head1 BUGS

Nah.  At least, I don't think so.  Well, I hope not.

=head1 AUTHOR

Reid Augustin, E<lt>reid@hellosix.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut


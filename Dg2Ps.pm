# $Id: Dg2Ps.pm 143 2005-06-03 21:05:57Z reid $

#   Dg2Ps
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

Games::Go::Dg2Ps - Perl extension to convert Games::Go::Diagrams to
PostScript.

=head1 SYNOPSIS

use Games::Go::Dg2Ps

 my $dg2ps = B<Games::Go::Dg2Ps-E<gt>new> (options);
 $dg2ps->convertDiagram($diagram);

=head1 DESCRIPTION

B<Games::Go::Dg2Ps> converts a L<Games::Go::Diagram> into PostScript.

=cut

use strict;
require 5.001;

package Games::Go::Dg2Ps;
eval { require PostScript::File; };   # is this module available?
if ($@) {
    die ("
    Dg2Ps needs the PostScript::File module, but it is not available.
    You can find PostScript::File in the same repository where you found
    Games::Go::Sgf2Dg, or from http://search.cpan.org/\n\n");
}

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

our %options = (
    boardSize       => 19,
    doubleDigits    => 0,
    coords          => 0,
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
    # PDF=specific options:
    pageSize        => 'Letter',
    topMargin       => 72 * .70,
    bottomMargin    => 72 * .70,
    leftMargin      => 72 * .70,
    rightMargin     => 72 * .70,
    text_fontName   => 'Times-Roman',
    text_fontSize   => 11,
    stone_fontName  => 'Courier-Bold',
    stone_fontSize  => 5,
    stone_width     => undef,
    stone_height    => undef,
    ps_debug        => 0,
    );

use constant Y_NUMBER_OFFSET => -0.5;

######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over 4

=item my $dg2ps = B<Games::Go::Dg2Ps-E<gt>new> (?options?)

A B<new> Games::Go::Dg2Ps takes the following options:

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

=item B<coords> =E<gt> true | false

Generates a coordinate grid.

Default: false

=item B<topLine>     =E<gt> number (Default: 1)

=item B<bottomLine>  =E<gt> number (Default: 19)

=item B<leftLine>    =E<gt> number (Default: 1)

=item B<rightLine>   =E<gt> number (Default: 19)

The edges of the board that should be displayed.  Any portion of the
board that extends beyond these numbers is not included in the
output.

=item B<diaCoords> =E<gt> sub { # convert $x, $y to Diagram coordinates }

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

=item B<print> =E<gt> sub { my ($dg2tex, @tex) = @_; ... }

A user defined subroutine to replace the default printing method.
This callback is called from the B<print> method (below) with the
reference to the B<Dg2TeX> object and a list of lines that are
part of the TeX diagram source.

=back

=head2 Dg2Ps-specific options:

=over 4

=item B<pageSize> =E<gt> 'page size'

May be one of:

=over 4

=item 'A0' - 'A9'

=item 'B0' - 'B10'

=item 'Executive'

=item 'Folio'

=item ’Half-Letter’

=item 'Letter'

=item ’US-Letter’

=item 'Legal

=item ’US-Legal’

=item 'Tabloid'

=item ’SuperB’

=item 'Ledger'

=item ’Comm #10 Envelope’

=item ’Envelope-Monarch’

=item ’Envelope-DL’

=item ’Envelope-C5’

=item ’EuroPostcard’

=back

Default: 'Letter'

=item B<topMargin>    =E<gt> points

=item B<bottomMargin> =E<gt> points

=item B<leftMargin>   =E<gt> points

=item B<rightMargin>  =E<gt> points

Margins are set in PostScript 'user space units' which are approximately
equivilent to points (1/72 of an inch).

Default for all margins: 72 * .70 (7/10s of an inch)

=item B<text_fontName>  =E<gt> 'font'  Default: 'Times-Roman',

=item B<stone_fontName> =E<gt> 'font'  Default: 'Courier-Bold'

Text and stone fonts names may be one of these (case sensitive):

=over 4

=item Courier

=item Courier-Bold

=item Courier-BoldOblique

=item Courier-Oblique

=item Helvetica

=item Helvetica-Bold

=item Helvetica-BoldOblique

=item Helvetica-Oblique

=item Times-Roman

=item Times-Bold

=item Times-Italic

=item Times-BoldItalic

=back

=item B<text_fontSize>  =E<gt> points

The point size for the comment text.  Diagram titles use this size
plus 4, and the game title uses this size plus 6.

Default: 11

=item B<stone_fontSize> =E<gt> points

The stone_fontSize determines the size of the stones and diagrams.
Stone size is chosen to allow up to three digits on a stone .  The
default stone_fontSize allows for three diagrams (with -coords) per
'letter' page if comments don't take up extra space below diagrams.

If B<doubleDigits> is specified, the stones and board are slightly
smaller (stone 100 may look a bit cramped).

Default: 5

=item B<stone_width> =E<gt> points

=item B<stone_height> =E<gt> points

The B<stone_width> and B<stone_height> determine the size of the
stones and diagrams.

If B<stone_width> is not explicitly set, it is calculated from the
B<stone_fontSize> to allow up to three digits on a stone .  The
default B<stone_fontSize> allows for three diagrams (with -coords)
per 'letter' page if comments don't take up extra space below
diagrams.  If B<doubleDigits> is specified, the stones and board are
slightly smaller (stone 100 may look a bit cramped).

If B<stone_height> is not explicitly set, it will be 1.05 *
B<stone_width>, creating a slightly rectangular diagram.

Default: undef - determined from B<stone_fontSize>

=item B<ps_debug> =#<gt> number from 0 to 2

When non-zero, code and subroutines are added to the PostScript
output to help debug the PostScript file.  This is very slightly
documented in L<PostScript::File>, but you'll probably need to read
through the PostScript output to make any use of it.

Default: 0

=back

=cut

sub new {
    my ($proto, %args) = @_;

    my $my = {};
    bless($my, ref($proto) || $proto);
    #$my->{stone_width} = 1;
    #$my->{stone_height} = 1;
    $my->{diagram_box_right} = 1;
    $my->{diagram_box_bottom} = 0;
    $my->{text_box_y_last} = 0;
    $my->{pre_init_print} = [];         # ref to empty array
    foreach (keys(%options)) {
        $my->{$_} = $options{$_};  # transfer default options
    }
    # transfer user args
    $my->configure(%args);
    return($my);
}

=item $dg2ps-E<gt>B<configure> (option =E<gt> value, ?...?)

Change Dg2Ps options from values passed at B<new> time.

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
            $my->{print} = sub { $_[0]->{ps}->add_to_page($_[1]) or
                                        die "Error writing to output file:$!\n"; };
        } else {
            require IO::File;
            $my->{filename} = $my->{file};
            $my->{file} = IO::File->new($my->{filename}) or
                die("Error opening $my->{filename}: $!\n");
            $my->{print} = sub { $_[0]->{ps}->add_to_page($_[1]) or
                                        die "Error writing to $_[0]->{filename}:$!\n"; };
        }
    }
    foreach (keys(%args)) {
        if (exists($options{$_})) {
            $my->{$_} = $args{$_};  # transfer user option
        } else {
            carp("Unknown option: $_");
        }
    }
    # make sure edges of the board don't exceed boardSize
    $my->{topLine}    = 1 if ($my->{topLine} < 1);
    $my->{leftLine}   = 1 if ($my->{leftLine} < 1);
    $my->{bottomLine} = $my->{boardSize} if ($my->{bottomLine} > $my->{boardSize});
    $my->{rightLine}  = $my->{boardSize} if ($my->{rightLine} > $my->{boardSize});
}

=item $dg2ps-E<gt>B<print> ($text ? , ... ?)

B<print>s raw PostScript code to B<file> as defined at B<new> time.

=cut

sub print {
    my ($my, @args) = @_;

    foreach my $arg (@args) {
        next unless (defined($arg) and
                     ($arg ne ''));
        if(exists($my->{ps})) {
            &{$my->{print}} ($my, $arg);
        } else {
            push(@{$my->{pre_init_print}}, @args);
        }
    }
}

=item $dg2ps-E<gt>B<printComment> ($text ? , ... ?)

Adds $text to the diagram comments.

=cut

sub printComment {
    my ($my, @args) = @_;

    foreach(@args) {
        $my->_flow_text($_);
    }
}

=item $dg2ps-E<gt>B<comment> ($comment ? , ... ?)

Inserts the PostScript comment character ('%') in front of each line of
each comment and B<print>s it to B<file>.

Note that this is I<not> the same as the B<printComment> method.

=cut

sub comment {
    my ($my, @comments) = @_;

    foreach my $c (@comments) {
        while ($c =~ s/([^\n]*)\n//) {
            $my->print("%$1\n");
        }
        $my->print("%$c\n") if ($c ne '');
    }
}

=item my $canvas = $dg2ps-E<gt>B<convertDiagram> ($diagram)

Converts a L<Games::Go::Diagram> into PostScript.

=cut

sub convertDiagram {
    my ($my, $diagram) = @_;

    my @name = $diagram->name;
    $name[0] = 'Unknown Diagram' unless(defined($name[0]));
    my $pageLabel = '?';
    if ($name[0] =~ m/^Variation\s*(\S*)/) {
        $pageLabel = "V$1";
    } elsif ($name[0] =~ m/^Diagram\s*(\S*)/) {
        $pageLabel = "D$1";
    }
    unless(exists($my->{ps})) {
        $my->_createPostScript;
        $my->{firstPage} = 1;
        $my->print(join("\n", @{$my->{pre_init_print}}));
    }

    $my->_next_diagram_box;      # get location for next diagram
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

    if (exists($propRef->{0}{N})) {
        $range .= "\n\n$propRef->{0}{N}";       # node name
    }
    $my->_preamble;
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

    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        foreach my $x ($my->{leftLine} ..  $my->{rightLine}) {
            $my->_convertIntersection($diagram, $x, $y);
        }
        if ($my->{coords}) {    # right-side coords
            $my->_createText(
                $my->_boardX($my->{rightLine} + 1), $my->_boardY($y) + Y_NUMBER_OFFSET,
                -text => $my->{boardSize} - $y + 1);
        }
    }
    # print bottom coordinates
    $my->_interlude;

    # now handle text associated with this diagram
    {
        local $my->{text_fontSize} = $my->{text_fontSize} + 6;
        unless(exists($my->{titleDone})) {       # first diagram only:
            $my->{titleDone} = 1;
            $my->convertProperties($diagram->property(0));      # any game-level properties?
        }
        $my->{text_fontSize} -= 4;
        # print the diagram title
        $my->printComment($my->convertText(join('', @name, $range, "\n")));

    }
    # the over-lay stones
    $my->_convertOverstones($diagram);
    $my->printComment("\n");
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
            $c .= join("\n", @comment);
            $my->printComment($my->convertText("$c\n"));
        }
    }
    $my->_postamble();
}

=item my $converted_text = $dg2ps-E<gt>B<convertText> ($text)

Converts $text into text for display (handles PostScript escape
sequences).

Returns the converted text.

=cut

sub convertText {
    my ($my, $text) = @_;

# PostScript escapes:
#   \\ backslash
#   \( left parenthesis
#   \) right parenthesis
#   \n line feed (LF)
#   \r carriage return (CR)
#   \t horizontal tab
#   \b backspace
#   \f form feed
#   \ddd character code ddd (octal)
    $text =~ s/([)(\\])/\\$1/gs;
    # turn single \n into single space.  multiple \n's are broken during _flow_text
    $text =~ s/([^\n])\n([^\n])/$1 $2/gs;
    $text =~ s/\r/\\r/gs;
    $text =~ s/\t/\\t/gs;
    # $text =~ s/\b/\\b/gs;     # hmmm, \b is word boundry in perl
    $text =~ s/\f/\\f/gs;

    return $text;
}

=item $title = $dg2ps-E<gt>B<convertProperties> (\%sgfHash)

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
        $my->{playerWhite} = $hash{PW};
        if(defined($hash{WR})) {
            push(@lines, "White: $hash{PW} $hash{WR}");  # PlayerWhite and WhiteRank
        } else {
            push(@lines, "White: $hash{PW}");            # PlayerWhite
        }
    }
    if (defined($hash{PB})) {
        $my->{playerBlack} = $hash{PB};
        if(defined($hash{BR})) {
            push(@lines, "Black: $hash{PB} $hash{BR}");  # PlayerBlack and BlackRank
        } else {
            push(@lines, "Black: $hash{PB}");            # PlayerBlack
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
        push(@lines, "Komi: $hash{KM}");
    }
    push(@lines, "Result: $hash{RE}") if (defined($hash{RE}));   # result
    push(@lines, "Time: $hash{TM}") if (defined($hash{TM}));     # time constraints
    foreach my $line (@lines) {
        next unless (defined($line));
        $my->printComment($my->convertText($line));
    }
}

=item $dg2ps-E<gt>B<close>

B<print>s final PostScript code to the output file and closes the
file.

=cut

sub close {
    my ($my) = @_;

    my $ps = $my->{ps}->output;
    if ((ref($my->{file}) eq 'GLOB') or
        (ref($my->{file}) eq 'IO::File')) {
        $my->{file}->print($ps);
        $my->{file}->close;
    }
    return $ps;
}

######################################################
#
#       Private methods
#
#####################################################

sub _convertOverstones {
    my ($my, $diagram) = @_;

    my @converted;

    return unless (@{$diagram->getoverlist});

    my ($color, $number, $otherColor);
    for (my $ii = 0; $ii < @{$diagram->getoverlist}; $ii++) {
        my $int = $diagram->getoverlist->[$ii];
        $my->{text_box_y} += $my->{text_fontSize};   # un-adjust for text line height
        $my->{text_box_y} -= $my->{stone_height} * 1.2;# adjust for stone height
        my $x = $my->{text_box_left};
        # all the overstones that were put on this understone:
        my $comma = 0;
        for (my $jj = 0; $jj < @{$int->{overstones}}; $jj += 2) {
            if ($comma ) {
                $my->_createText(
                    $x, $my->{text_box_y},
                    -anchor => 'sw',
                    -font     => $my->{text_fontName},
                    -fontSize => $my->{text_fontSize},
                    -text => ',');
                $x += $my->{text_fontSize} * $my->_string_width($my->{text_fontName}, ',');
            }
            if ($my->{text_box_right} - $x < 3 * $my->{stone_width}) {
                $my->{text_box_y} -= $my->{stone_height} * 1.2;  # drop to next line
                $x = $my->{text_box_left};
                $jj -= 2;
                $comma = 0;
                next;   # try again
            }
            $color = $int->{overstones}[$jj];
            local $my->{stoneOffset} = $my->{offset};   # turn off doubleDigits
            $number = $my->_checkStoneNumber($int->{overstones}[$jj+1]);
            # draw the overstone
            my $left = $x;
            my $right = $x + $my->{stone_width};
            my $top = $my->{text_box_y} + $my->{stone_height};
            my $bottom = $my->{text_box_y};
            $my->_createOval(
                $left, $top, $right, $bottom,
                -fill => $color,);
            # put the number on it
            $otherColor = ($color eq 'black') ? 'white' : 'black';
            $my->_createText(
                $x + $my->{stone_width} / 2,
                $my->{text_box_y} + Y_NUMBER_OFFSET + $my->{stone_height} / 2,
                -fill => $otherColor,
                -text => $number);
            $x += $my->{stone_width};
            $comma = 1;
        }
        # the 'at' stone
        if (exists($int->{black})) {
            $color = 'black';
            $otherColor = 'white';
        } elsif (exists($int->{white})) {
            $color = 'white';
            $otherColor = 'black';
        } else {
            carp("Oops: understone is not black or white? " .
                 "This isn't supposed to be possible!");
            next;
        }
        # at
        $my->_createText(
            $x, $my->{text_box_y} + Y_NUMBER_OFFSET,
            -anchor => 'sw',
            -font     => $my->{text_fontName},
            -fontSize => $my->{text_fontSize},
            -text => ' at');
        $x += $my->{text_fontSize} * $my->_string_width($my->{text_fontName}, ' at');
        # draw the at-stone
        my $left = $x;
        my $right = $x + $my->{stone_width};
        my $top = $my->{text_box_y} + $my->{stone_height};
        my $bottom = $my->{text_box_y};
        $my->_createOval(
            $left, $top, $right, $bottom,
           -fill => $color,);
        if (exists($int->{number})) {
            # put the number on it
            $my->_createText(
                $x + $my->{stone_width} / 2,
                $my->{text_box_y} + Y_NUMBER_OFFSET + $my->{stone_height} / 2,
                -fill => $otherColor,
                -text => $my->_checkStoneNumber($int->{number}));
        } elsif (exists($int->{mark})) {
            # draw the mark on it
            # triangle has top Y; left, right X; and bottom Y
            my $hCenter = $x + ($my->{stone_width} / 2);
            my $top = $my->{text_box_y} + $my->{stone_height};
            my $left = $hCenter - (.433 * $my->{stone_width});         # cos(30) = .866
            my $right = $hCenter + (.433 * $my->{stone_width});        # cos(30) = .866
            my $bottom = $my->{text_box_y} + ($my->{stone_height} / 4);     # sin(30) = .5
            $my->_createLine(
                $hCenter, $top,
                $right,   $bottom,
                $left,    $bottom,
                $hCenter, $top,
                -fill => $otherColor);
        } else {
            my $mv = '';
            $mv .= " black node=$int->{black}" if (exists($int->{black}));
            $mv .= " white node=$int->{white}" if (exists($int->{black}));
            carp("Oops: understone$mv is not numbered or marked? " .
                 "This isn't supposed to be possible!");
        }
        $x += $my->{stone_width};
        if ($ii < @{$diagram->getoverlist} - 1) {
            $my->_createText(
                $x, $my->{text_box_y},
                -anchor => 'sw',
                -font     => $my->{text_fontName},
                -fontSize => $my->{text_fontSize},
                -text => ',');
        }
        $my->{text_box_y} -= $my->{text_fontSize};   # re-adjust for text line height
        $my->{text_box_y_last} = $my->{text_box_y};
        $my->{text_box_used} = 1;
    }
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

# convert intersection hash from $diagram.
sub _convertIntersection {
    my ($my, $diagram, $x, $y) = @_;

    my $int = $diagram->get(&{$my->{diaCoords}}($x, $y));
    my ($stone, $color, $otherColor);
    if (exists($int->{black})) {
        $color = 'black';
        $otherColor = 'white';
    }elsif (exists($int->{white})) {
        $color = 'white';
        $otherColor = 'black';
    }
    if (exists($int->{number})) {
        $stone = $my->_checkStoneNumber($int->{number}); # numbered stone
    } elsif (exists($int->{mark})) {
        $stone = 'mark';                        # marked stone
        unless(defined($color)) {
            carp("Can't mark empty intersction");
        }
    } elsif (exists($int->{label})) {
        $stone = $int->{label};             # labeled stone or intersection
    }

    if (defined($color)) {      # there is a black or white stone on this intersection
        my $left = $my->_boardX($x) - $my->{stone_width} / 2;
        my $right = $left + $my->{stone_width};
        my $top = $my->_boardY($y) - $my->{stone_height} / 2;
        my $bottom = $top + $my->{stone_height};
        $my->_createOval(
            $left, $top, $right, $bottom,
            -fill => $color,);
        if (defined($stone)) {
            if ($stone eq 'mark') {
                $my->_drawMark($otherColor, $x, $y);
            } else {
                $my->_createText(
                    $my->_boardX($x), $my->_boardY($y) + Y_NUMBER_OFFSET,
                    -fill => $otherColor,
                    -text => $stone);
            }
        }
    } else {                    # no stone here
        if (defined($stone)) {
            # create some whitespace to draw label on
            my $left = $my->_boardX($x) - $my->{stone_width} / 3;
            my $right = $left + $my->{stone_width} / 1.5;
            my $top = $my->_boardY($y) - $my->{stone_height} / 3;
            my $bottom = $top + $my->{stone_height} / 1.5;
            $my->_createOval(
                $left, $top, $right, $bottom,
                -fill    => 'white',
                -outline => 'white',);
            $my->_createText(
                $my->_boardX($x),
                $my->_boardY($y) + Y_NUMBER_OFFSET,
                -fontSize => $my->{stone_fontSize} + 2,
                -text => $stone);
        } elsif (exists($int->{hoshi})) {
            $my->_drawHoshi($x, $y);
        }
    }
}

sub _drawMark {
    my ($my, $color, $x, $y) = @_;

    # triangle has top Y; left, right X; and bottom Y
    my $hCenter = $my->_boardX($x);
    my $top = $my->_boardY($y) + ($my->{stone_height} / 2);
    my $left = $hCenter - (.433 * $my->{stone_width});         # cos(30) = .866
    my $right = $hCenter + (.433 * $my->{stone_width});        # cos(30) = .866
    my $bottom = $my->_boardY($y) - ($my->{stone_height} / 4); # sin(30) = .5
    $my->_createLine(
        $hCenter, $top,
        $right,   $bottom,
        $left,    $bottom,
        $hCenter, $top,
        -fill => $color);
}

sub _drawHoshi {
    my ($my, $x, $y) = @_;

    my $size = $my->{stone_width} * 0.08;   # 8% size of a stone
    $size = 1 if $size <= 0;
    my $left = $my->_boardX($x) - $size;
    my $right = $left + 2 * $size;
    my $top = $my->_boardY($y) - $size;
    my $bottom = $top + 2 * $size;
    $my->_createOval(
        $left, $top, $right, $bottom,
        -fill => 'black');
}

# use preamble to build the empty board
sub _preamble {
    my ($my) = @_;

    # vertical lines
    my $top = $my->_boardY($my->{topLine});
    $top += $my->{stone_height} / 2 unless($my->{topLine} <= 1);
    my $bot = $my->_boardY($my->{bottomLine});
    $bot -= $my->{stone_height} / 2 unless($my->{bottomLine} >= $my->{boardSize});
    for (my $x = $my->{leftLine}; $x <= $my->{rightLine}; $x++) {
        my $cx = $my->_boardX($x);
        $my->_createLine($cx, $top, $cx, $bot);
    }
    # horizontal lines
    my $left = $my->_boardX($my->{leftLine});
    $left -= $my->{stone_width} / 2 unless($my->{leftLine} <= 1);
    my $right = $my->_boardX($my->{rightLine});
    $right += $my->{stone_width} / 2 unless($my->{rightLine} >= $my->{boardSize});
    my $cy;
    for (my $y = $my->{topLine}; $y <= $my->{bottomLine}; $y++) {
        $cy = $my->_boardY($y);
        $my->_createLine($left, $cy, $right, $cy);
    }
    return unless ($my->{coords});
    $cy -= $my->{stone_height};
    for (my $x = $my->{leftLine}; $x <= $my->{rightLine}; $x++) {
        my $coord = (qw(A B C D E F G H J K L M N O P Q R S T U V W X Y Z))[$x - 1];
        next unless(defined($coord));
        $my->_createText(
            $my->_boardX($x), $cy,
            -text => $coord);
    }
}

# nothing to do for PostScript _interlude
sub _interlude {
    my ($my) = @_;

}

# this one's pretty easy too
sub _postamble {
    my ($my) = @_;

}

sub _boardX {
    my ($my, $x) = @_;

    return $my->{diagram_box_left} + ($x - $my->{leftLine} + 0.5) * $my->{stone_width};
}

sub _boardY {
    my ($my, $y) = @_;

    return $my->{diagram_box_top} - ($y - $my->{topLine} + 0.5) * $my->{stone_height};
}

# imitate a Tk::Canvas createText call
sub _createText {
    my ($my, $x, $y, %args) = @_;

    $my->_set_rgb(delete($args{-fill}));
    my $text = delete($args{-text});
    my $font = delete($args{-font}) || $my->{stone_fontName};
    my $fontSize = delete($args{-fontSize}) || $my->{stone_fontSize};
    my $x_off = 0;
    my $y_off = 1;          # anchor offset - default to sw
    my $vspace = 3.6 * $fontSize;
    if (exists($args{-anchor})) {
        if ($args{-anchor} eq 'sw') {
        } else {
            carp ("Unknown anchor in _createText: $args{-anchor}");
        }
        delete ($args{-anchor});
    } else {
        $x_off = -0.5;
        $y_off = 0.5;          # center anchor
    }
    foreach (keys(%args)) {
        carp ("Unknown args key in _createText: $_");
    }
    $my->print("/$font findfont $fontSize scalefont setfont\n");
    $my->print("$x $y [\n[($text)]\n] $vspace $x_off $y_off 0 DrawText\n");
}

# imitate a Tk::Canvas createOval call
sub _createOval {
    my ($my, $x1, $y1, $x2, $y2, %args) = @_;

    $my->_set_rgb(delete($args{-fill}));
    my $outline = delete($args{-outline});
    foreach (keys(%args)) {
        carp ("Unknown args key in _createOval: $_");
    }
    my $half_w = ($x2 - $x1) / 2;
    my $half_h = ($y2 - $y1) / 2;
    my $x = $x1 + $half_w;
    my $y = $y1 + $half_h;
    $my->print("gsave matrix currentmatrix\n");
    $my->print("$x $y translate $half_w $half_h scale 1 0 moveto 0 0 1 0 360 arc\n");
    $my->print("setmatrix gsave fill grestore\n");
    $my->_set_rgb($outline);
    $my->print("stroke grestore\n");
}

# imitate a Tk::Canvas createLine call
sub _createLine {
    my ($my, $x1, $y1, @args) = @_;

    my @points;
    while (@args) {
       last if ($args[0] =~ m/[^-\d\.]/);
       push(@points, shift(@args), shift(@args));
    }
    my %args = @args;
    $my->_set_rgb(delete($args{-fill}));
    foreach (keys(%args)) {
        carp ("Unknown args key in _createLine: $_");
    }
    $my->print("newpath $x1 $y1 moveto\n");
    while (@points) {
        $my->print(shift(@points) . " " . shift(@points) . " lineto\n");
    }
    $my->print("stroke\n");
}

sub _set_rgb {
    my ($my, $color) = @_;
    
    my ($r, $g, $b) = (0, 0, 0);
    $color = 'black' unless (defined($color));
    $color = lc($color);
    if ($color eq 'white') {
        ($r, $g, $b) = (1, 1, 1);
    } elsif ($color eq 'red') {
        ($r, $g, $b) = (1, 0, 0);
    } elsif ($color eq 'green') {
        ($r, $g, $b) = (0, 1, 0);
    } elsif ($color eq 'blue') {
        ($r, $g, $b) = (0, 0, 1);
    } elsif ($color eq 'black') {
        ($r, $g, $b) = (0, 0, 0);
    } else {
        carp ("unknown color $color in _set_rgb");
        $color = 'black';
    }
    $my->print("$r $g $b setrgbcolor\n");   # set fill color
}

sub _createPostScript {

    my ($my) = @_;

    my $ps = $my->{ps} = new PostScript::File(
        paper    => $my->{pageSize},
        clipping => 1,
        order    => 'ascend',
        debug    => $my->{ps_debug},
        );

    $my->{page_left}   = 0 + $my->{leftMargin};
    $my->{page_right}  = $ps->get_width - $my->{rightMargin};
    $my->{page_top}    = $ps->get_height - $my->{topMargin};
    $my->{page_bottom} = 0 + $my->{bottomMargin};

    # figure out the font and line width and height
    my $fontScale = $my->{fontScale} = 0.4;  # approximate size in points when fontSize == 1
    unless(defined($my->{stone_width})) {
        $my->{stone_width} = $my->{doubleDigits} ?
                                $fontScale * 4.5 :    # need space for two digits (and 100)
                                $fontScale * 5.0;     # need space for three digits
        $my->{stone_width} *= $my->{stone_fontSize};
    }
    my $hLines = (1 + $my->{rightLine}  - $my->{leftLine});
    my $vLines = (1 + $my->{bottomLine} - $my->{topLine});
    my $pageH = ($my->{page_top} - $my->{page_bottom});
    my $pageW = ($my->{page_right} - $my->{page_left});
    if ($my->{stone_width}  * $hLines  > $pageW) {
        my $newW = $pageW / $hLines;
        carp "stone_width of $my->{stone_width} won't fit on the page.  I'm setting it to $newW\n";
        $my->{stone_width} = $newW;
    }
    unless(defined($my->{stone_height})) {
        $my->{stone_height} = $my->{stone_width} * 1.05;   # 95% aspect ratio
    }
    if ($my->{stone_height}  * $vLines  > $pageH) {
        my $newH = $pageH / $vLines;
        carp "stone_width of $my->{stone_height} won't fit on the page.  I'm setting it to $newH\n";
        $my->{stone_height} = $newH;
    }

    $my->{diagram_width}  = $my->{stone_width}  * $hLines;
    $my->{diagram_height} = $my->{stone_height} * $vLines;
    if ($my->{coords}) {
        $my->{diagram_width}  += $my->{stone_width};
        $my->{diagram_height} += $my->{stone_height};
    }
    $my->{ps}->add_function('My_Functions', <<END_FUNCTIONS);
%
% Note: these functions are 'borrowed' from the Tk::Canvas
% postscript conversion method.
%
/cstringshow {
    {
	dup type /stringtype eq
	{ show } { glyphshow }
	ifelse
    }
    forall
} bind def

/cstringwidth {
    0 exch 0 exch
    {
	dup type /stringtype eq
	{ stringwidth } {
	    currentfont /Encoding get exch 1 exch put (\001) stringwidth
        }
	ifelse
	exch 3 1 roll add 3 1 roll add exch
    }
    forall
} bind def

% x y strings spacing xoffset yoffset justify DrawText --
% This procedure does all of the real work of drawing text.  The
% color and font must already have been set by the caller, and the
% following arguments must be on the stack:
%
% x, y -	Coordinates at which to draw text.
% strings -	An array of strings, one for each line of the text item,
%		in order from top to bottom.
% spacing -	Spacing between lines.
% xoffset -	Horizontal offset for text bbox relative to x and y: 0 for
%		nw/w/sw anchor, -0.5 for n/center/s, and -1.0 for ne/e/se.
% yoffset -	Vertical offset for text bbox relative to x and y: 0 for
%		nw/n/ne anchor, +0.5 for w/center/e, and +1.0 for sw/s/se.
% justify -	0 for left justification, 0.5 for center, 1 for right justify.
%
% Also, when this procedure is invoked, the color and font must already
% have been set for the text.

/DrawText {
    /justify exch def
    /yoffset exch def
    /xoffset exch def
    /spacing exch def
    /strings exch def

    % First scan through all of the text to find the widest line.

    /lineLength 0 def
    strings {
	cstringwidth pop
	dup lineLength gt {/lineLength exch def} {pop} ifelse
	newpath
    } forall

    % Compute the baseline offset and the actual font height.

    gsave
    0 0 moveto (TXygqPZ) false charpath
    pathbbox dup /baseline exch def
    exch pop exch sub /height exch def pop
    newpath

    % Translate coordinates first so that the origin is at the upper-left
    % corner of the text's bounding box. Remember that x and y for
    % positioning are still on the stack.

    translate
    lineLength xoffset mul
    strings length 1 sub spacing mul height add yoffset mul translate

    % Now use the baseline and justification information to translate so
    % that the origin is at the baseline and positioning point for the
    % first line of text.

    justify lineLength mul baseline neg translate

    % Iterate over each of the lines to output it.  For each line,
    % compute its width again so it can be properly justified, then
    % display it.

    strings {
	dup cstringwidth pop
	justify neg mul 0 moveto
	cstringshow
	0 spacing neg translate
    } forall
    grestore
} bind def

END_FUNCTIONS
}

# handle text reflow
sub _flow_text {
    my ($my, $text) = @_;

    my $width = 0;
    my @line = ();
    my $token = my $space = '';
    until (($text eq '') and
           ($token eq '')) {
        if ($token eq '') {
            $text =~ s/^(\s*)(\S*)//s;      # whitespace, then non-whitespace
            $space = $1;
            $token = $2;
            $space =~ s/ +/ /gs;        # turn multiple spaces into single space
            $space =~ s/ \n/\n/gs;      # remove preceding and intervening blanks
            $space =~ s/\n /\n/gs;      # and trailing blanks
        }
        my $tokenWidth = $my->{text_fontSize} * $my->_string_width($my->{text_fontName}, "$space$token");
        if (($space =~ m/\n/) or
            ($width + $tokenWidth > $my->{text_box_width})) {
            if ($width) {
                # put collected tokens on current line
                $my->_flow_text_lf(join('', @line));
                $width = 0;
                @line = ();
                $space =~ s/\n//;       # remove one LF (if there's one here)
            } else {            # no @line, but token is too long
                # put first part of token on current line:
                $token = $my->_flow_force_break($token); 
            }
            while ($space =~ s/\n//) {
                $my->_flow_text_lf(''); # extra LFs?
            }
            $space = '';    # no preceding space on next line
        } else {
            push(@line, "$space$token");
            $width += $tokenWidth;
            $token = '';
        }
    }
    $my->_flow_text_lf(join('', @line)) if (@line);
}

# force a break in a chunk that's too wide for the box, return the remainder
sub _flow_force_break {
    my ($my, $text) = @_;

    my $idx = 0;
    my $width = 0;
    while (($width < $my->{text_box_width}) and
           ($idx < length($text))) {
        my $c = substr($text, $idx, 1);
        $width += $my->{text_fontSize} * $my->_string_width($my->{text_fontName}, $c);
        $idx++;
    }
    $my->_flow_text_lf(substr($text, 0, $idx - 1));
    return substr($text, $idx)
}

# print a line, then update box data to reflect a line-feed
sub _flow_text_lf {
    my ($my, $text) = @_;

# print " flow $text\n";
    $my->_createText($my->{text_box_left}, $my->{text_box_y},
        -anchor   => 'sw',
        -font     => $my->{text_fontName},
        -fontSize => $my->{text_fontSize},
        -text     => $text);
    if ($text =~ m/\S/) {       # non-whitespace here
        $my->{text_box_y_last} = $my->{text_box_y};
        $my->{text_box_used} = 1;
    }
    $my->{text_box_y} -= 1.2 * $my->{text_fontSize};
    if ($my->{text_box_y} <= $my->{text_box_bottom}) {
        $my->_next_text_box();
    }
}

# figure out where the next diagram box should be.
sub _next_diagram_box {
    my ($my) = @_;

# print "next diagram box\n";
    $my->{text_box_state} = 0;  # next text box should be to right of diagram
    # is there enough space under the latest text?
    my $prev_bottom = $my->{diagram_box_bottom};
    if ($my->{text_box_used} and
        ($my->{text_box_y_last} < $prev_bottom)) {
        $prev_bottom = $my->{text_box_y_last};  # text is below bottom of diagram
        $prev_bottom -= $my->{stone_height};     # extra space between text and next diagram
    }
    # some space between diagrams
    $prev_bottom -= $my->{stone_height} unless ($prev_bottom == $my->{page_top});
    my $need = $my->{diagram_height} - $my->{stone_height} + $my->{page_bottom};
    if ($prev_bottom > $need) { # enough space on this page still
        $my->{diagram_box_top}    = $prev_bottom;
    } else {                    # need a new page
        $my->_next_page;
        $my->{diagram_box_top}    = $my->{page_top};
    }
    $my->{diagram_box_left}   = $my->{page_left};
    $my->{diagram_box_right}  = $my->{diagram_box_left} + $my->{diagram_width};
    $my->{diagram_box_bottom} = $my->{diagram_box_top} - $my->{diagram_height};
    $my->_next_text_box;     # need a new text box for this diagram
}

# figure out where the next text box should be.  box may be to the right of a
#       diagram, underneath a diagram, or it may be a new page.
sub _next_text_box {
    my ($my) = @_;

# print "next text box: ";
    $my->{text_box_state}++;
    if ($my->{text_box_state} == 1) {   # try for the area to the right of the diagram
        my $min_text = 'revive his dead stones';        # at least this wide...
        my $min_width = $my->{text_fontSize} * $my->_string_width($my->{text_fontName}, $min_text);
        my $dia_right = $my->{diagram_box_right} + $my->{stone_width};
        if ($my->{page_right} - ($dia_right + 10) < $min_width) {
            $my->{text_box_bottom} = $my->{diagram_box_bottom};
            $my->_next_text_box;                 # not enough room, try next box
        } else {
            $my->{text_box_left}   = $dia_right;
            $my->{text_box_right}  = $my->{page_right} - 10;
            $my->{text_box_top}    = $my->{diagram_box_top} - $my->{stone_height};
            $my->{text_box_bottom} = $my->{diagram_box_bottom} - $my->{text_fontSize} * 1.2;;
            $my->{text_box_bottom} = $my->{page_bottom} if ($my->{text_box_bottom} < $my->{page_bottom});
# print "right\n";
        }
    } elsif ($my->{text_box_state} == 2) {      # try for the area under the diagram
        $my->{text_box_left}   = $my->{page_left} + 10;
        $my->{text_box_right}  = $my->{page_right} - 10;
        $my->{text_box_top}    = $my->{text_box_y};
        while ($my->{text_box_top} > $my->{text_box_bottom}) {
            $my->{text_box_top}    -= $my->{text_fontSize} * 1.2;
        }
        $my->{text_box_bottom} = $my->{page_bottom};
        if ($my->{text_box_top} < $my->{page_bottom}) {
            $my->_next_text_box;                 # not enough space, try next
        }
# print "under\n";
    } else {                                    # gotta start a new page...
# print "new page\n";
        $my->_next_page;
        $my->{text_box_left}   = $my->{page_left} + 10;
        $my->{text_box_right}  = $my->{page_right} - 10;
        $my->{text_box_top}    = $my->{page_top} - $my->{stone_height};
        $my->{text_box_bottom} = $my->{page_bottom};
        $my->{diagram_box_bottom} = $my->{page_top};    # no diagram on this page
    }
    $my->{text_box_width} = $my->{text_box_right} - $my->{text_box_left};
    $my->{text_box_y} = $my->{text_box_top};
    $my->{text_box_used} = 0;
}

# measure string width in points
sub _string_width {
    my ($my, $font, $text) = @_;

    my $w = 0;
    for (my $ii = 0; $ii < length($text); $ii++) {
        $w++;
        $ii++ if (substr($text, $ii, 1) eq '\\');       # skip escape chars
    }
    return $my->{fontScale} * $w;       # well, approximately...
}

# Add a new page which inherits its attributes from $root
my $page = 0;
sub _next_page {
    my ($my) = @_;

    $page++;
# print "next page($page)\n";
    $my->{ps}->newpage unless(exists($my->{firstPage}));
    delete($my->{firstPage});
    # set width to .3 points, line join mode to rounded corners
    $my->print(".3 setlinewidth 1 setlinejoin\n");
    $my->{text_box_y} = $my->{text_box_y_last} = $my->{page_top} - $my->{stone_height};
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

Bugs?  In I<my> code?

=head1 AUTHOR

Reid Augustin, E<lt>reid@hellosix.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut


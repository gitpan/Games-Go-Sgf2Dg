# $Id: Dg2Tk.pm 143 2005-06-03 21:05:57Z reid $

#   Dg2Tk
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

Games::Go::Dg2Tk - Perl extension to convert Games::Go::Diagrams to
perl/Tk windows.

=head1 SYNOPSIS

use Games::Go::Dg2Tk

 my $dg2tk = B<Games::Go::Dg2Tk-E<gt>new> (options);
 my $canvas = $dg2tk->convertDiagram($diagram);

=head1 DESCRIPTION

A Games::Go::Dg2Tk object converts a L<Games::Go::Diagram> object
into Tk::Canvas item.  The B<close> method calls Tk::MainLoop to
dispays the collection of Canvases.

Bindings for the normal editing keys: Up, Down, Next (PageDown) and
Prior (PageUp) traverse the NoteBook tabs.  Tab and Shift-tab also
work as expected.

Left and Right keys select the previous or next NoteBook tab, but
don't display it.  Space and Enter (carriage return) display the
selected tab.

=cut

use strict;
require 5.001;

package Games::Go::Dg2Tk;
use Tk;
use Tk::NoteBook;
use Tk::Canvas;
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
    );

use constant Y_NUMBER_OFFSET => 1;

######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over 4

=item my $dg2tk = B<Games::Go::Dg2Tk-E<gt>new> (?options?)

Any options passed to Dg2Tk that are not recognized are passed in
turn to the Tk::Canvas widgets as they are created (which may
cause errors if Tk::Canvas also does not recognize them).

A B<new> Games::Go::Dg2Tk takes the following options:

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

=back

=cut

sub new {
    my ($proto, %args) = @_;

    my $my = {};
    bless($my, ref($proto) || $proto);
    $my->{fontWidth} = 1;
    $my->{fontHeight} = 1;
    $my->{lineWidth} = 1;
    $my->{lineHeight} = 1;
    $my->{rightEdge} = 1;
    $my->{bottomEdge} = 1;
    foreach (keys(%options)) {
        $my->{$_} = $options{$_};  # transfer default options
    }
    $my->{mw} = MainWindow->new;
    my $nb = $my->{notebook} = $my->{mw}->NoteBook();
    $nb->pack(
            -expand => 'true',
            -fill   => 'both');
    $nb->configure(-takefocus => 1); # for tab-traversal
    # tab traversal bindings:
    $nb->bind( '<Tab>', sub { $nb->raise($nb->info('focusnext')); });
    $nb->bind( '<Next>', sub { $nb->raise($nb->info('focusnext')); });
    #$nb->bind( '<Right>', sub { $nb->raise($nb->info('focusnext')); });
    $nb->bind( '<Down>', sub { $nb->raise($nb->info('focusnext')); });
    $nb->bind( '<Shift-Tab>', sub { $nb->raise($nb->info('focusprev')); });
    $nb->bind( '<Prior>', sub { $nb->raise($nb->info('focusprev')); });
    #$nb->bind( '<Left>', sub { $nb->raise($nb->info('focusprev')); });
    $nb->bind( '<Up>', sub { $nb->raise($nb->info('focusprev')); });
    # bizzare - looks like we need this too:
    $nb->bind( '<<LeftTab>>', sub { $nb->raise($nb->info('focusprev')); });
    # transfer user args
    $my->configure(%args);
    return($my);
}

=item $dg2tk-E<gt>B<configure> (option =E<gt> value, ?...?)

Change Dg2Tk options from values passed at B<new> time.

=cut

sub configure {
    my ($my, %args) = @_;

    foreach (keys(%args)) {
        if (exists($options{$_})) {
            $my->{$_} = $args{$_};  # transfer user option
        } else {
            $my->{canvasOpts}{$_} = $args{$_};  # assume it's a canvas option
        }
    }
    # make sure edges of the board don't exceed boardSize
    $my->{topLine}    = 1 if ($my->{topLine} < 1);
    $my->{leftLine}   = 1 if ($my->{leftLine} < 1);
    $my->{bottomLine} = $my->{boardSize} if ($my->{bottomLine} > $my->{boardSize});
    $my->{rightLine}  = $my->{boardSize} if ($my->{rightLine} > $my->{boardSize});
}

=item $dg2tk-E<gt>B<print> ($text ? , ... ?)

For most Dg2 converters, B<print> inserts diagram source code (TeX,
ASCII, whatever) directly into the diagram source stream.  Since Tk
diplays the diagrams immediately, there is no concept of a source
stream, so B<print> just generates a warning.

=cut

sub print {
    my ($my, @args) = @_;

    carp("->print(...) does nothing");
}

=item $dg2tk-E<gt>B<printComment> ($text ? , ... ?)

Adds $text to the diagram comments.

=cut

sub printComment {
    my ($my, @args) = @_;

    foreach(@args) {
        chomp;
        foreach my $line (@{$my->_commentSplit($_)}) {
            $my->{currentBoard}->createText(2 * $my->{fontWidth}, $my->{textY},
                                            -anchor => 'sw',
                                            -text   => $line);
            $my->{textY} += (1.2 * $my->{fontHeight});
        }
    }
}

sub _commentSplit {
    my ($my, $line) = @_;

    my @massagedLines;
    my $charsPerLine = ($my->{rightEdge} / $my->{fontWidth});
    $line =~ s/\t/    /g;       # turn all tabs into 4 spaces
    my $prevIdx = 0;
    my $prevShortLine = 1;
    my $length = length($line);
    while ($prevIdx < $length) {
        my $break;
        if ($length <= $prevIdx + $charsPerLine) {
            $break = $length;
        } else {
            $break = rindex($line, ' ', $prevIdx + $charsPerLine); # space nearest next break point
            $break = $prevIdx + $charsPerLine if ($break < $prevIdx); # no space? force a break anyway
        }
        my $nIdx = index($line, "\n", $prevIdx);                # first newline
        my $nnIdx;                                              # second newline
        if ($nIdx < 0) {                 # no newline?
            $nnIdx = $nIdx = $length;
        } else {
            $nnIdx = index($line, "\n", $nIdx + 1);             # another newline?
            $nnIdx = $length if ($nnIdx < 0);                   # not found?
        }
        if ($nIdx - $prevIdx == 0) {
            $prevShortLine = 1;
            push(@massagedLines, '');
            $prevIdx++;
            next;
        }
        my $shortLine = ($nnIdx < $break);      # two newlines before break
        if ($shortLine) {
            my $l;
            if ($nIdx == $prevIdx) {            # hmm, undef or ''?  let's be safe
                $l = '';
            } else {
                $l = substr($line, $prevIdx, $nIdx - $prevIdx);
            }
            $prevIdx = $nIdx + 1;
            unless($prevShortLine) {
                $l .= ' ';
                while (($prevIdx < $nnIdx) and     # delete spaces at front of next line
                       (substr($line, $prevIdx, 1) eq ' ')) {
                    $prevIdx++;
                }
                $l .= substr($line, $prevIdx, $nnIdx - $prevIdx);
                $prevIdx = $nnIdx + 1;
            }
            push (@massagedLines, $l);
        } elsif ($nIdx < $break) {              # one newline before break
            my $l = substr($line, $prevIdx, $nIdx - $prevIdx);
            $prevIdx = $nIdx + 1;               # remove the newline
            $break++;
            $l .= ' ';
            while (($prevIdx < $break) and     # delete spaces at front of next line
                   (substr($line, $prevIdx, 1) eq ' ')) {
                $prevIdx++;
            }
            $l .= substr($line, $prevIdx, $break - $prevIdx);
            push (@massagedLines, $l);
            $prevIdx = $break;
        } else {                                # no newlines before break - force a break
            push (@massagedLines, substr($line, $prevIdx, $break - $prevIdx));
            $prevIdx = $break;
        }
        while (($prevIdx < $length) and     # delete spaces at front of next line
               (substr($line, $prevIdx, 1) eq ' ')) {
            $prevIdx++;
        }
        $prevShortLine = $shortLine;
    }
    return \@massagedLines;
}

=item $dg2tk-E<gt>B<comment> ($comment ? , ... ?)

For most Dg2 converters, B<comment> inserts comments into the
diagram source code (TeX, ASCII, whatever).  Since Tk diplays the
diagrams immediately, there is no concept of a source stream, so
B<comment> does nothing.


=cut

sub comment {
    my ($my, @comments) = @_;

    # carp("->comment(...) does nothing");
}

=item my $canvas = $dg2tk-E<gt>B<convertDiagram> ($diagram)

Converts a I<Games::Go::Diagram> into a Tk::Canvas widget.  Returns
a reference to the Canvas.  The Canvas is also added to the
Tk::NoteBook collection of diagrams that are displayed (at B<close>
time).

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
    $my->{currentPage} = $my->{notebook}->add(++$my->{pageNum}, -label => $pageLabel);
    # $my->{currentPage}->pack( # Yikes! packing notebook pages is a # bug!!!
    #         -expand => 'true',
    #         -fill   => 'both');
    my $scroller = $my->{currentPage}->Scrolled('Canvas',
                                                 -scrollbars => 'osoe',
                                                 -takefocus => 0,
                                                 %{$my->{canvasOpts}});
    $scroller->pack(
            -expand => 'true',
            -fill   => 'both');
    $my->{currentBoard} = $scroller->Subwidget('scrolled');
    push (@{$my->{diagrams}}, $scroller);
    $my->{currentBoard}->pack(
            -expand => 'true',
            -fill   => 'both');

    if($my->{fontWidth} == 1) {
        my $idx = $my->{currentBoard}->createText(0, 0, -text => 'AbcDefGhiJkl');
        my ($l, $u, $r, $b) = $my->{currentBoard}->bbox($idx);
        $my->{fontWidth} = abs($r - $l) / 12;
        $my->{fontHeight} = abs($b - $u);
        $my->{lineWidth} = ($my->{fontWidth} * 4);           # need space for three digits
        $my->{lineHeight} = ($my->{lineWidth} * 1.05);       # 95% aspect ratio
        $my->{currentBoard}->delete($idx);
    }
    $my->{rightEdge} = $my->_boardX($my->{rightLine}) + $my->{lineWidth};
    $my->{rightEdge} += 1 if ($my->{coords});
    $my->{textY} = $my->{bottomLine} + 1;
    $my->{textY} += 1 if ($my->{coords});
    $my->{textY} = $my->_boardY($my->{textY}) + $my->{fontHeight};

    $my->convertProperties($diagram->property(0));      # any game-level properties?
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
    my ($diaHeight, $diaWidth) = (($my->{bottomLine} - $my->{topLine} + 1), ($my->{rightLine} - $my->{leftLine} + 1));
    $my->_preamble($diaHeight, $diaWidth);
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

    # print the diagram title
    $my->printComment(join('', @name, $range, "\n"));
    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        foreach my $x ($my->{leftLine} ..  $my->{rightLine}) {
            $my->_convertIntersection($diagram, $x, $y);
        }
        if ($my->{coords}) {    # right-side coords
            $my->{currentBoard}->createText($my->_boardX($my->{rightLine} + 1),
                                            $my->_boardY($y),
                                            -text => $my->{boardSize} - $y + 1);
        }
    }
    # print bottom coordinates
    $my->_interlude($diaWidth, $diaHeight);

    # deal with the over-lay stones
    $my->_convertOverstones($diagram);
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
    $my->{bottomEdge} = $my->{textY} + $my->{fontHeight};
    $my->{bottomEdge} += $my->{lineHeight} if ($my->{coords});
    $my->{currentBoard}->configure(-scrollregion => [0, 0, $my->{rightEdge}, $my->{bottomEdge}],);
    unless($my->{resizeDone}) {
        $my->{currentBoard}->configure(-width => $my->{rightEdge} + 5,
                                       -height => $my->{bottomEdge} + 5);
        $my->{resizeDone} = 1;
    }
    $my->{mw}->update;
    unless(exists($my->{canvas_bg})) {
        $my->{canvas_bg} = $my->{currentBoard}->cget('-background');
        # on the first board, we may not be able to color background items correctly:
        $my->{currentBoard}->itemconfigure('bg', -fill => $my->{canvas_bg});
    }
    Tk::focus($my->{notebook});
}

=item my $converted_text = $dg2tk-E<gt>B<convertText> ($text)

Converts $text into text for display - gee, that's not very hard.
In fact, this method simply returns whatever is passed to it.  This
is really just a place-holder for more complicated converters.

Returns the converted text.

=cut

sub convertText {
    my ($my, $text) = @_;

    return $text;
}

=item $title = $dg2tk-E<gt>B<convertProperties> (\%sgfHash)

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
            push(@lines, "White: $hash{PW} $hash{WR}");  # PlayerWhite and WhiteRank
        } else {
            push(@lines, "White: $hash{PW}");            # PlayerWhite
        }
    }
    if (defined($hash{PB})) {
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
    my ($title)='';
    foreach my $line (@lines) {
        next unless (defined($line));
        $line =~ s/\\([][)(\\])/$1/g;                           # change escaped chars to non-escaped
        $title .= "$line\n";
    }
    if($title ne '') {
        $my->printComment("\n$title\n\n");
    }
}

=item $dg2tk-E<gt>B<close>

B<print>s any final text to the diagram (currently none) and closes
the dg2tk object.  Also closes B<file> if appropriate.

=cut

sub close {
    my ($my) = @_;

    $my->{mw}->MainLoop;      # never to return...
}

=item $dg2tk-E<gt>B<notebook>

Returns a reference to the notebook of L<Tk::Canvas> objects.

=cut

sub notebook {
    my ($my) = @_;

    return $my->{notebook}; # the notebook object
}

=item $dg2tk-E<gt>B<diagrams>

Returns a reference to the list of L<Tk::Canvas> objects that make
up the Tk::NoteBook of diagrams.  Note that each item in the list is
actually a L<Tk::Scrolled> object, the actual Tk::Canvas object is:

    my $canvas = $dg2tk->diagrams->[$idx]->Subwidget('scrolled');

=cut

sub diagrams {
    my ($my) = @_;

    return $my->{diagrams}; # the list of diagrams
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
        $my->{textY} += $my->{lineHeight} - $my->{fontHeight};  # adjust for stone height
        my $x = 2 * $my->{fontWidth};
        # all the overstones that were put on this understone:
        for (my $jj = 0; $jj < @{$int->{overstones}}; $jj += 2) {
            if ($jj > 0 ) {
                $my->{currentBoard}->createText($x, $my->{textY},
                                                -anchor => 'sw',
                                                -text => ',');
                $x += $my->{fontWidth};
            }
            $color = $int->{overstones}[$jj];
            local $my->{stoneOffset} = $my->{offset};   # turn off doubleDigits
            $number = $my->_checkStoneNumber($int->{overstones}[$jj+1]);
            # draw the overstone
            my $left = $x;
            my $right = $x + $my->{lineWidth};
            my $top = $my->{textY} - $my->{lineHeight};
            my $bottom = $my->{textY};
            $my->{currentBoard}->createOval($left, $top, $right, $bottom,
                            -fill => $color,
                            );
            # put the number on it
            $otherColor = ($color eq 'black') ? 'white' : 'black';
            $my->{currentBoard}->createText(
                            $x + ($my->{lineWidth} / 2),
                            $my->{textY} + Y_NUMBER_OFFSET - ($my->{lineHeight} / 2),
                            -fill => $otherColor,
                            -text => $number
                            );
            $x += $my->{lineWidth};
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
        $my->{currentBoard}->createText($x, $my->{textY},
                                        -anchor => 'sw',
                                        -text => ' at ');
        $x += 3 * $my->{fontWidth};
        # draw the at-stone
        my $left = $x;
        my $right = $x + $my->{lineWidth};
        my $top = $my->{textY} - $my->{lineHeight};
        my $bottom = $my->{textY};
        $my->{currentBoard}->createOval($left, $top, $right, $bottom,
                        -fill => $color,
                        );
        if (exists($int->{number})) {
            # put the number on it
            $my->{currentBoard}->createText(
                            $x + ($my->{lineWidth} / 2),
                            $my->{textY} + Y_NUMBER_OFFSET - ($my->{lineHeight} / 2),
                            -fill => $otherColor,
                            -text => $my->_checkStoneNumber($int->{number})
                            );
        } elsif (exists($int->{mark})) {
            # draw the mark on it
            # triangle has top Y; left, right X; and bottom Y
            my $hCenter = $x + ($my->{lineWidth} / 2);
            my $top = $my->{textY} - $my->{lineHeight};
            my $left = $hCenter - (.433 * $my->{lineWidth});         # cos(30) = .866
            my $right = $hCenter + (.433 * $my->{lineWidth});        # cos(30) = .866
            my $bottom = $my->{textY} - ($my->{lineHeight} / 4);     # sin(30) = .5
            $my->{currentBoard}->createLine(
                               $hCenter, $top,
                               $right,   $bottom,
                               $left,    $bottom,
                               $hCenter, $top,
                               -fill => $otherColor
                            );
        } else {
            my $mv = '';
            $mv .= " black node=$int->{black}" if (exists($int->{black}));
            $mv .= " white node=$int->{white}" if (exists($int->{black}));
            carp("Oops: understone$mv is not numbered or marked? " .
                 "This isn't supposed to be possible!");
        }
        $x += $my->{lineWidth};
        if ($ii < @{$diagram->getoverlist} - 1) {
            $my->{currentBoard}->createText($x, $my->{textY},
                                            -anchor => 'sw',
                                            -text => ',');
        }
        $my->{textY} += (1.2 * $my->{fontHeight});
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

    if (defined($color)) {
        my $left = $my->_boardX($x) - $my->{lineWidth} / 2;
        my $right = $left + $my->{lineWidth};
        my $top = $my->_boardY($y) - $my->{lineHeight} / 2;
        my $bottom = $top + $my->{lineHeight};
        $my->{currentBoard}->createOval($left, $top, $right, $bottom,
            -fill => $color,
            );

        if (defined($stone)) {
            if ($stone eq 'mark') {
                $my->_drawMark($otherColor, $x, $y);
            } else {
                $my->{currentBoard}->createText(
                    $my->_boardX($x),
                    $my->_boardY($y) + Y_NUMBER_OFFSET,
                    -fill => $otherColor,
                    -text => $stone
                    );
            }
        }
    } else {
        if (exists($int->{hoshi})) {
            $my->_drawHoshi($x, $y);
        }
        if (defined($stone)) {
            # create some whitespace to draw label on
            my $left = $my->_boardX($x) - $my->{lineWidth} / 3;
            my $right = $left + $my->{lineWidth} / 1.5;
            my $top = $my->_boardY($y) - $my->{lineHeight} / 3;
            my $bottom = $top + $my->{lineHeight} / 1.5;
            $my->{currentBoard}->createOval(
                $left, $top, $right, $bottom,
                -fill    => $my->{canvas_bg},
                -outline => undef,
                -tags    => ['bg']);
            $my->{currentBoard}->createText(
                $my->_boardX($x),
                $my->_boardY($y) + Y_NUMBER_OFFSET,
                -text => $stone);
        }
    }
}

sub _drawMark {
    my ($my, $color, $x, $y) = @_;

    # triangle has top Y; left, right X; and bottom Y
    my $hCenter = $my->_boardX($x);
    my $top = $my->_boardY($y) + Y_NUMBER_OFFSET - ($my->{lineHeight} / 2);
    my $left = $hCenter - (.433 * $my->{lineWidth});         # cos(30) = .866
    my $right = $hCenter + (.433 * $my->{lineWidth});        # cos(30) = .866
    my $bottom = $my->_boardY($y) + Y_NUMBER_OFFSET + ($my->{lineHeight} / 4); # sin(30) = .5
    $my->{currentBoard}->createLine(
                       $hCenter, $top,
                       $right,   $bottom,
                       $left,    $bottom,
                       $hCenter, $top,
                       -fill => $color
                    );
}

sub _drawHoshi {
    my ($my, $x, $y) = @_;

    my $size = ($my->{lineWidth} * 0.05);   # 10% size of a stone
    $size = 1 if $size <= 0;
    my $left = $my->_boardX($x) - $size;
    my $right = $left + 2 * $size;
    my $top = $my->_boardY($y) - $size;
    my $bottom = $top + 2 * $size;
    $my->{currentBoard}->createOval($left, $top, $right, $bottom,
                    -fill => 'black'
                    );
}

# use preamble to build the empty board
sub _preamble {
    my ($my, $diaHeight, $diaWidth) = @_;

    # vertical lines
    my $top = $my->_boardY($my->{topLine});
    $top -= $my->{lineHeight} / 2 unless($my->{topLine} <= 1);
    my $bot = $my->_boardY($my->{bottomLine});
    $bot += $my->{lineHeight} / 2 unless($my->{bottomLine} >= $my->{boardSize});
    for (my $x = $my->{leftLine}; $x <= $my->{rightLine}; $x++) {
        my $cx = $my->_boardX($x);
        $my->{currentBoard}->createLine($cx, $top, $cx, $bot);
    }
    # horizontal lines
    my $left = $my->_boardX($my->{leftLine});
    $left -= $my->{lineWidth} / 2 unless($my->{leftLine} <= 1);
    my $right = $my->_boardX($my->{rightLine});
    $right += $my->{lineWidth} / 2 unless($my->{rightLine} >= $my->{boardSize});
    my $cy;
    for (my $y = $my->{topLine}; $y <= $my->{bottomLine}; $y++) {
        $cy = $my->_boardY($y);
        $my->{currentBoard}->createLine($left, $cy, $right, $cy);
    }
    return unless ($my->{coords});
    $cy += $my->{lineHeight};
    for (my $x = $my->{leftLine}; $x <= $my->{rightLine}; $x++) {
        my $coord = (qw(A B C D E F G H J K L M N O P Q R S T U V W X Y Z))[$x - 1];
        next unless(defined($coord));
        $my->{currentBoard}->createText($my->_boardX($x), $cy,
                                 -text => $coord);
    }
}

# nothing to do for Tk _interlude
sub _interlude {
    my ($my, $diaWidth, $diaHeight) = @_;

}

# this one's pretty easy too
sub _postamble {
    my ($my) = @_;

}

sub _boardX {
    my ($my, $x) = @_;

    return (($x - $my->{leftLine} + 1.5) * $my->{lineWidth});
}

sub _boardY {
    my ($my, $y) = @_;

    return (($y - $my->{topLine} + 1.5) * $my->{lineHeight});
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

We ain't got to show you no stinkin' bugs!

=head1 AUTHOR

Reid Augustin, E<lt>reid@hellosix.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut


# $Id: Dg2ASCII.pm 143 2005-06-03 21:05:57Z reid $

#   Dg2ASCII
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

Games::Go::Dg2ASCII - Perl extension to convert Games::Go::Diagrams to ASCII diagrams

=head1 SYNOPSIS

use Games::Go::Dg2ASCII

 my $dg2ascii = B<Games::Go::Dg2ASCII-E<gt>new> (options);
 my $ascii = $dg2ascii->convertDiagram($diagram);

=head1 DESCRIPTION

A Games::Go::Dg2ASCII object converts a L<Games::Go::Diagram> object
into ASCII diagrams.

=cut

use strict;
require 5.001;

package Games::Go::Dg2ASCII;
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

use constant TOPLEFT     => ' +--';
use constant TOPRIGHT    => '-+  ';
use constant TOP         => '----';
use constant BOTTOMLEFT  => ' +--';
use constant BOTTOMRIGHT => '-+  ';
use constant BOTTOM      => '----';
use constant LEFT        => ' |  ';
use constant RIGHT       => ' |  ';
use constant MIDDLE      => ' +  ';
use constant HOSHI       => ' *  ';
use constant WHITE       => " O  ";    # numberless white stone
use constant BLACK       => " X  ";    # numberless black stone
use constant MARKEDWHITE => " @  ";    # marked white stone
use constant MARKEDBLACK => " #  ";    # marked black stone

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

######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over 4

=item my $dg2ascii = B<Games::Go::Dg2ASCII-E<gt>new> (?options?)

A B<new> Games::Go::Dg2ASCII takes the following options:

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

=item B<file> =E<gt> 'filename' | $descriptor | \$string | \@array

If B<file> is defined, the ASCII diagram is dumped into the target.
The target can be any of:

=over 4

=item filename

The filename will be opened using IO::File->new.  The filename
should include the '>' or '>>' operator as described in 'perldoc
IO::File'.  The ASCII diagram is written into the file.

=item descriptor

A file descriptor as returned by IO::File->new, or a \*FILE
descriptor.  The ASCII diagram is written into the file.

=item reference to a string scalar

The ASCII diagram is concatenated to the end of the string.

=item reference to an array

The ASCII diagram is split on "\n" and each line is pushed onto the array.

=back

Default: undef

=item B<print> =E<gt> sub { my ($dg2ascii, @lines) = @_; ... }

A user defined subroutine to replace the default printing method.
This callback is called from the B<print> method (below) with the
reference to the B<Dg2ASCII> object and a list of lines that are
part of the ASCII diagram lines.

=back

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
        croak("I don't understand option $_\n") unless(exists($options{$_}));
        $my->{$_} = $args{$_};  # transfer user option
    }
    # make sure edges of the board don't exceed boardSize
    $my->{topLine}    = 1 if ($my->{topLine} < 1);
    $my->{leftLine}   = 1 if ($my->{leftLine} < 1);
    $my->{bottomLine} = $my->{boardSize} if ($my->{bottomLine} > $my->{boardSize});
    $my->{rightLine}  = $my->{boardSize} if ($my->{rightLine} > $my->{boardSize});
}

=item $dg2ascii-E<gt>B<print> ($text ? , ... ?)

B<print>s the input $text directly to B<file> as defined at B<new>
time.  Whether or not B<file> was defined, B<print> accumulates the
$text for later retrieval with B<converted>.

=cut

sub print {
    my ($my, @args) = @_;

    foreach my $arg (@args) {
        $my->{converted} .= $arg;
        &{$my->{print}} ($my, $arg);
    }
}

=item my $ascii = $dg2ascii-E<gt>B<converted> ($replacement)

Returns the entire ASCII diagram converted so far for the
B<Dg2ASCII> object.  If $replacement is defined, the accumulated
ASCII is replaced by $replacement.

=cut

sub converted {
    my ($my, $text) = @_;

    $my->{converted} = $text if (defined($text));
    return ($my->{converted});
}

=item $dg2ascii-E<gt>B<comment> ($comment ? , ... ?)

Inserts the comment character (which is nothing for ASCII) in front
of each line of each comment and B<print>s it to B<file>.

=cut

sub comment {
    my ($my, @comments) = @_;

    foreach my $c (@comments) {
        while ($c =~ s/([^\n]*)\n//) {
            $my->print("$1\n");
        }
        $my->print("$c\n") if ($c ne '');
    }
}

=item my $dg2ascii-E<gt>B<convertDiagram> ($diagram)

Converts a I<Games::Go::Diagram> into ASCII.  If B<file> was defined
in the B<new> method, the ASCII is dumped into the B<file>.  In any
case, the ASCII is returned as a string scalar.

=cut

sub convertDiagram {
    my ($my, $diagram) = @_;

    unless($my->{firstDone}) {
        $my->print("
Black -> X   Marked black -> #   Labeled black -> Xa, Xb
White -> O   Marked white -> @   Labeled white -> Oa, Ob\n");
        $my->{firstDone} = 1;
    }
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

    if (exists($propRef->{0}{N})) {
        $range .= "\n\n$propRef->{0}{N}";       # node name
    }
    # get some measurements based on font size
    my ($diaHeight, $diaWidth) = (($my->{bottomLine} - $my->{topLine} + 1), ($my->{rightLine} - $my->{leftLine} + 1));
    if ($my->{coords}) {
        $diaWidth += 4;
        $diaHeight += 2;
    }
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
    $my->print(join('', @name, $range, "\n"));
    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        foreach my $x ($my->{leftLine} ..  $my->{rightLine}) {
            $my->_convertIntersection($diagram, $x, $y);
        }
        if ($my->{coords}) {    # right-side coords
            $my->print($my->{boardSize} - $y + 1);
        }
        $my->print("\n");
        if ($y < $my->{bottomLine}) {
            if ($my->{rightLine} - $my->{leftLine} > 1) {
                $my->print(LEFT,
                           '    ' x ($my->{rightLine} - $my->{leftLine} - 1),
                           RIGHT,
                           "\n");
            } else {
                $my->print(LEFT, "\n");       # doesn't seem very likely!
            }
        }
    }
    # print coordinates
    $my->_interlude($diaWidth, $diaHeight);

    # deal with the over-lay stones
    $my->_convertOverstones($diagram);
    $my->print("\n");
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
            $my->print($my->convertText("$c\n"));
        }
    }
    $my->_postamble();
}

=item my $ascii = $dg2ascii-E<gt>B<convertText> ($text)

Converts $text into ASCII code - gee, that's not very hard.  In
fact, this method simply returns whatever is passed to it.  This is
really just a place-holder for more complicated converters.

Returns the converted text.

=cut

sub convertText {
    my ($my, $text) = @_;

    return $text;
}

=item $title = $dg2ascii-E<gt>B<convertProperties> (\%sgfHash)

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
        $my->print("\n$title\n");
    }
}

=item $dg2ascii-E<gt>B<close>

B<print>s any final text to the diagram (currently none) and closes
the dg2ascii object.  Also closes B<file> if appropriate.

=cut

sub close {
    my ($my) = @_;

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

sub _convertOverstones {
    my ($my, $diagram) = @_;

    my @converted;

    foreach my $int (@{$diagram->getoverlist()}) {
        my $overStones = '';
        for(my $ii = 0; $ii < @{$int->{overstones}}; $ii += 2) {
            # all the overstones that were put on this understone:
            my $overColor = $int->{overstones}[$ii];
            my $overNumber = $int->{overstones}[$ii+1];
            $overStones .= ", " if ($overStones ne '');
            local $my->{stoneOffset} = $my->{offset};
            $overStones .= $my->_checkStoneNumber($overNumber);
        }
        my $atStone = '';
        if (exists($int->{number})) {
            # numbered stone in text
            $atStone = $my->_checkStoneNumber($int->{number});
        } else {
            unless (exists($int->{mark})) {
                my $mv = '';
                $mv .= " black node=$int->{black}" if (exists($int->{black}));
                $mv .= " white node=$int->{white}" if (exists($int->{black}));
                carp("Oops: understone$mv is not numbered or marked? " .
                     "This isn't supposed to be possible!");
            }
            if (exists($int->{black})) {
                $atStone = '#';        # marked black stone in text
            }elsif (exists($int->{white})) {
                $atStone = '@';        # marked white stone in text
            } else {
                carp("Oops: understone is not black or white? " .
                     "This isn't supposed to be possible!");
            }
        }
        # collect all the overstones in the diagram
        push(@converted, "$overStones at $atStone");
    }
    return '' unless(@converted);
    $my->print(join(",\n", @converted), "\n");
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


sub _formatNumber {
    my ($my, $number) = @_;

    return " $number  " if ($number < 10);
    return  "$number  " if ($number < 100);
    return   "$number ";
}

# get text for intersection hash from $diagram.
sub _convertIntersection {
    my ($my, $diagram, $x, $y) = @_;

    my $int = $diagram->get(&{$my->{diaCoords}}($x, $y));
    my $stone;
    if (exists($int->{number})) {
        $stone = $my->_formatNumber($my->_checkStoneNumber($int->{number})); # numbered stone
    } elsif (exists($int->{mark})) {
        if (exists($int->{black})) {
            $stone = MARKEDBLACK;                       # marked black stone
        }elsif (exists($int->{white})) {
            $stone = MARKEDWHITE;                       # marked white stone
        } else {
            carp("Can't mark empty intersction");
        }
    } elsif (exists($int->{label})) {
        if (exists($int->{black})) {
            $stone = ' ' . BLACK . lc($int->{label}) . ' ';     # labeled black stone
        } elsif (exists($int->{white})) {
            $stone = ' ' . WHITE . lc($int->{label}) . ' ';     # labeled white stone
        } else {
            $stone = " $int->{label}  ";                        # labeled intersection
        }
    } elsif (exists($int->{white})) {
        $stone = WHITE;       # numberless white stone
    } elsif (exists($int->{black})) {
        $stone = BLACK;        # numberless black stone
    }

    unless (defined($stone)) {
        if (exists($int->{hoshi})) {
            $stone = HOSHI;
        } else {
            $stone = $my->_underneath($x, $y);
        }
    }
    $my->print($stone);
}

# return the appropriate font char for the intersection
sub _underneath {
    my ($my, $x, $y) = @_;

    if ($y <= 1) {
        return TOPLEFT if ($x <= 1);            # upper left corner
        return TOPRIGHT if ($x >= $my->{boardSize}); # upper right corner
        return TOP;                             # upper side
    } elsif ($y >= $my->{boardSize}) {
        return BOTTOMLEFT if ($x <= 1);         # lower left corner
        return BOTTOMRIGHT if ($x >= $my->{boardSize}); # lower right corner
        return BOTTOM;                          # lower side
    }
    return LEFT if ($x <= 1);                   # left side
    return RIGHT if ($x >= $my->{boardSize});   # right side
    return MIDDLE;                              # somewhere in the middle
}

# don't need any preamble for text diagrams
sub _preamble {
    my ($my, $diaHeight, $diaWidth) = @_;

    return;
}

sub _interlude {
    my ($my, $diaWidth, $diaHeight) = @_;

    # print coordinates along the bottom
    if ($my->{coords}) {
        my ($l, $r) = ($my->{leftLine}, $my->{rightLine});
        $my->print(' ',
        join('   ', (qw(A B C D E F G H J K L M N O P Q R S T U V W X Y Z))[($l - 1) .. ($r - 1)]), "\n");
    }
}

# this one's pretty easy too
sub _postamble {
    my ($my) = @_;

    $my->print("\n\n");
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

Seems unlikely.

=head1 AUTHOR

Reid Augustin, E<lt>reid@hellosix.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut


# $Id: Dg2Mp.pm 143 2005-06-03 21:05:57Z reid $

#   Dg2Mp
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

Games::Go::Dg2Mp - Perl extension to convert Games::Go::Diagrams to
John Hobby's MetaPost (which is adapted from Donald Knuth's
Metafont).

=head1 SYNOPSIS

use Games::Go::Dg2Mp

 my $dg2mp = B<Games::Go::Dg2Mp-E<gt>new> (options);
 $dg2mp->convertDiagram($diagram);

=head1 DESCRIPTION

A Games::Go::Dg2Mp object converts a L<Games::Go::Diagram> object
into a TeX (.tex) and a MetaPost (.mp) file.  The MetaPost file
contains figures for each of the diagrams and overstones required to
make the complete game diagram.  Running MetaPost (mpost or possibly
mp) on the .mp file creates a set of figure files, each of which is
an Encapsulated PostScript figure.  Running TeX (tex) on the .tex
file creates a .dvi file which tries to include the Encapsulated
PostScript figures.  Running dvips on the .dvi file (from TeX)
creates the final PostScript (.ps) file containing the complete game
diagram.

See 'man mpost' (or possibly 'man 'mp') for more details of the
overall MetaPost system and environment.

=cut

use strict;
require 5.001;

package Games::Go::Dg2Mp;
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

use constant DEFAULT_FONT   => "cmssbx10";      # default font for numerals
use constant BIGNUMBER_FONT => "cmr10";         # font for numbers > 99
use constant ITALIC_FONT    => "cmbxti10";      # font for letters
use constant MP_FUNCS  => "
% how to draw a stone and a triangle:
path stone, triangle;
stone = fullcircle xscaled (stone_width) yscaled(stone_height);
triangle = ((0, .45)--(.39, -.225)--(-.39,-.225)--cycle) xscaled (stone_width) yscaled (stone_height);

% convert board coords to real coords:
% note: goboard lines are numbered with 1 at the top and increasing
%    towards the bottom of the page.  vline_count allows us to
%    invert Y for PostScript coordinates that increase going up
vardef boardXY (expr m, n) =
   ((m - 0.5) * stone_width, (vline_count - n - 0.5) * stone_height)
enddef;

% white stone at m, n with label or number k
vardef whitestone(expr m, n, k) =
fill stone shifted boardXY(m, n) withcolor white;
label(k, boardXY(m, n)) withcolor black;
draw stone shifted boardXY(m, n);
enddef;

% black stone at m, n with label or number k
vardef blackstone(expr m, n, k) =
fill stone shifted boardXY(m, n) withcolor black;
label(k, boardXY(m, n)) withcolor white;
enddef;

% black stone at m, n with triangle mark
vardef triangleblackstone(expr m, n) =
blackstone(m, n, \"\");
draw triangle shifted boardXY(m, n) withcolor white;
enddef;

% white stone at m, n with triangle mark
vardef trianglewhitestone(expr m, n) =
whitestone(m, n, \"\");
draw triangle shifted boardXY(m, n) withcolor black;
enddef;

% label an empty intersection at m, n with k
vardef labelintersection(expr m, n, k) =
% create some blank space under the label
unfill stone scaled 0.6 shifted boardXY(m, n);
label(k, boardXY(m, n)) withcolor black;
enddef;

";


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
    # Mp=specific options:
    stone_fontName  => 'cmssbx10',
    stone_fontSize  => 8,
    stone_width     => undef,
    stone_height    => undef,
    );

use constant NORMAL_MACROS =>
"\\magnification=1200
\\newdimen\\diagdim
\\newdimen\\fulldim
\\newbox\\diagbox
\\newbox\\captionbox\n";

use constant SIMPLE_MACROS =>
"\\magnification=1200
\\raggedbottom
\\parindent=0pt\n";

use constant TWO_COLUMN_MACROS =>
"\\magnification=1200
\\input gotcmacs
\\raggedbottom
\\tolerance=10000
\\parindent=0pt\n";


######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over 4

=item my $dg2mp = B<Games::Go::Dg2Mp-E<gt>new> (?options?)

A B<new> Games::Go::Dg2Mp takes the following options:

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

=item B<print> =E<gt> sub { my ($dg2mp, @tex) = @_; ... }

A user defined subroutine to replace the default printing method.
This callback is called from the B<print> method (below) with the
reference to the B<Dg2Mp> object and a list of lines that are
part of the TeX diagram source.

=back

=head2 Dg2Mp-specific options:

=over 4

=item B<stone_fontName> =E<gt> 'font'  Default: 'cmssbx10'

Quoting from the discussion on fonts in section 7 of _A User's
Manual for MetaPost_ (by John D. Hobby):

"...the new font name should be something that TEX would understand
since MetaPost gets height and width information by reading the tfm
file. (This is explained in The TEXbook. [5] ) It should be possible
to use built-in PostScript fonts, but the names for them are
system-dependent. Some systems may use rptmr or ps-times-roman
instead of Times-Roman. A TEX font such as cmr10 is a little
dangerous because it does not have a space character or certain
ASCII symbols. In addition, MetaPost does not use the ligatures and
kerning information that comes with a TEX font."

=item B<stone_fontSize> =E<gt> points

The stone_fontSize determines the size of the stones and diagrams.
Stone size is chosen to allow up to three digits on a stone.

If B<doubleDigits> is specified, the stones and board are slightly
smaller (stone 100 may look a bit cramped).

Default: 8

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

=item $dg2mp-E<gt>B<configure> (option =E<gt> value, ?...?)

Change Dg2Mp options from values passed at B<new> time.

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

=item $dg2mp-E<gt>B<print> ($tex ? , ... ?)

B<print>s raw TeX code to B<file> as defined at B<new> time.
Whether or not B<file> was defined, B<print> accumulates the TeX
code for later retrieval with B<converted>.
The TeX output filename is derived from the MetaPost filename by
changing the .mp extension to .tex.

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
        $my->print("\\input epsf\n");
    }
    foreach my $arg (@args) {
        $my->{converted} .= $arg;
        &{$my->{print}} ($my, $arg);
    }
}


=item $dg2mp-E<gt>B<print> ($tex ? , ... ?)

B<print>s raw MetaPost code to MetaPost output file (as defined at
->B<new> or ->B<configure> time).

=cut

sub mpprint {
    my ($my, @args) = @_;

    $my->{mpFile}->print(@args);
}

=item my $tex = $dg2mp-E<gt>B<converted> ($replacement_tex)

Returns the TeX source code converted so far for the B<Dg2Mp>
object.  If $replacement_tex is defined, the accumulated TeX source
code is replaced by $replacement_tex.

=cut

sub converted {
    my ($my, $tex) = @_;

    $my->{converted} = $tex if (defined($tex));
    return ($my->{converted});
}

=item $dg2mp-E<gt>B<comment> ($comment ? , ... ?)

Inserts the TeX comment character ('%') in front of each line of
each comment and B<print>s it to B<file>.

=cut

sub comment {
    my ($my, @comments) = @_;

    if (exists($my->{mpFile})) {
        if (exists($my->{pre_comments})) {
            my @c = @{delete($my->{pre_comments})};
            $my->comment(@c);
            local $my->{file} = $my->{mpFile};  # also copy to MetaPost output file
            $my->comment(@c);
        } else {
            local $my->{macrosDone} = 1;        # allow comments before one-time init
            foreach my $c (@comments) {
                while ($c =~ s/([^\n]*)\n//) {
                    $my->print("%$1\n");
                }
                $my->print("%$c\n") if ($c ne '');
            }
        }
    } else {
        push(@{$my->{pre_comments}}, @comments);
    }
}

=item my $tex_source = $dg2mp-E<gt>B<convertDiagram> ($diagram)

Converts a I<Games::Go::Diagram> into TeX/MetaPost.  If B<file> was
defined in the B<new> method, the TeX source is dumped into the
B<file>.tex and the MetaPost source into B<file>.mp.  In any case,
the TeX source is returned as a string scalar.

=cut

sub convertDiagram {
    my ($my, $diagram) = @_;

    $my->_createMp($diagram) unless(exists($my->{mpFile}));
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
    $my->mpprint("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    $my->mpprint("%  Start of ", @name, "$range\n");
    $my->mpprint("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");

    if (exists($propRef->{0}{N})) {
        $range .= "\n\n$propRef->{0}{N}";       # node name
    }
    $my->_preamble();
    $my->mpprint("draw_board;\n");
    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        my $x;
        for ($x = $my->{leftLine}; $x <= $my->{rightLine}; $x++) {
            $my->_convertIntersection($diagram, $x, $y);
        }
        if ($my->{coords}) {    # right-side coords
            my $coord = $my->{boardSize} - $y + 1;
            $my->mpprint("label(\"$coord\", boardXY($x, $y)) withcolor black;\n");
        }
    }
    # the bottom coordinates
    $my->_interlude();
    $my->mpprint("endfig;\n\n");

    # now handle text associated with this diagram
    unless(exists($my->{titleDone})) {       # first diagram only:
        $my->{titleDone} = 1;
        $my->convertProperties($diagram->property(0));      # any game-level properties?
    }
    # the diagram title
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
    my $title = join('', @name, $range);
    # print the diagram title
    if (($my->{twoColumn})or ($my->{simple})) {
        $my->print("\n$title\\hfil\\break\n");
    } else {
        # BUGBUG my $hangIndentLines = int(1 + $my->{bigFonts} + ($diaHeight - (1+.2*$my->{bigFonts})*$my->{gap})/ $my->{fontSize});
        $my->print("\\vfil\n",
                "\\setbox\\captionbox=\\vbox{\\tolerance=10000\\vglue-8pt\n",
                "\\parindent=0pt\\parskip=8pt\\vglue6pt\\lineskip=0pt\\baselineskip=12pt\n",
       # BUGBUG "\\hangindent $diaWidth pt",
       # BUGBUG "\\hangafter-$hangIndentLines\n",
                "\\noindent$title\\hfil\\break\\hfil\\break\n");
    }

    # deal with the over-lay stones
    $my->_convertOverstones($diagram);
    if ($my->{twoColumn}) {
        $my->print("\n");
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
            $my->print($my->convertText($c));
        }
    }
    if (($my->{twoColumn})or ($my->{simple})) {
    } else {
        $my->print("}\n");
    }
    $my->_postamble();
}

=item my $tex = $dg2mp-E<gt>B<convertText> ($text)

Converts $text into TeX code by changing certain characters that are
not available in TeX cmr10 font, and by converting \n\n into
\hfil\break.  B<convertText> behavior is modified by B<texComments>
and B<simple> options.

Returns the converted text.

=cut

sub convertText {
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

=item $tex_title = $dg2mp-E<gt>B<convertProperties> (\%sgfHash)

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

=item $dg2mp-E<gt>B<close>

B<print> the TeX closer (\bye) and close the dg2mp object.  Also
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
    $my->mpprint("end;\n");
    $my->{mpFile}->close;
}

######################################################
#
#       Private methods
#
#####################################################

sub _createMp {
    my ($my, $diagram) = @_;

    $my->{mpFile} = $my->{file};
    my $texName = $my->{filename} || 'sgf2mp';
    $texName =~ s/\.mp$//;
    $texName =~ s/>//g;
    $my->{mpfigname} = $texName;
    $texName = ">$texName.tex";
    $my->{file} = IO::File->new($texName) or
        die ("Couldn't open TeX file $texName: $!");
    $my->comment();                 # print comments so far to both files
    my $fontScale = $my->{fontScale} = 0.4;  # approximate size in points when fontSize == 1
    unless(defined($my->{stone_width})) {
        $my->{stone_width} = $my->{doubleDigits} ?
                                $fontScale * 4.5 :    # need space for two digits (and 100)
                                $fontScale * 5.0;     # need space for three digits
        $my->{stone_width} *= $my->{stone_fontSize};
    }
    $my->{stone_height} = $my->{stone_width} * 1.05 unless(defined($my->{stone_height}));
    $my->mpprint("defaultfont:=\"$my->{stone_fontName}\";\n",
                 "defaultscale := $my->{stone_fontSize}pt/fontsize defaultfont;\n",
                 "numeric stone_width, stone_height, vline_count;\n",
                 "stone_width = $my->{stone_width};\n",
                 "stone_height = $my->{stone_height};\n",
                 "vline_count = 1 + $my->{bottomLine} - $my->{topLine};\n",
                 "\n",
                 );
    $my->mpprint(MP_FUNCS);         # meta-post prolog
    $my->mpprint("% draw board\n",
                 "vardef draw_board =\n",
                 );
    $my->mpprint("% draw hoshi points\n",
                 "pickup pencircle scaled 1.5;\n",
                 );
    foreach my $y ($my->{topLine} .. $my->{bottomLine}) {
        foreach my $x ($my->{leftLine} .. $my->{rightLine}) {
            my $int = $diagram->get(&{$my->{diaCoords}}($x, $y));
            if (exists($int->{hoshi})) {
                $my->mpprint("drawdot (boardXY($x, $y));\n");
            }
        }
    }
    $my->mpprint("% draw lines\n",
                 "pickup pencircle scaled 0.3;\n",
                 );
    my $left = $my->{leftLine};
    $left -= 0.5 if ($my->{leftLine} != 1);
    my $right = $my->{rightLine};
    $right += 0.5 if ($my->{rightLine} != $my->{boardSize});
    $my->mpprint("for i = $my->{topLine}  upto $my->{bottomLine}:\n",
                 "    draw boardXY($left, i) -- boardXY($right, i);\n",
                 "endfor\n",
                 );
    my $top = $my->{topLine};
    $top -= 0.5 if ($my->{topLine} != 1);
    my $bottom = $my->{bottomLine};
    $bottom += 0.5 if ($my->{bottomLine} != $my->{boardSize});
    $my->mpprint("for i = $my->{leftLine}  upto $my->{rightLine}:\n",
                 "    draw boardXY(i, $top) -- boardXY(i, $bottom);\n",
                 "endfor\n",
                 "enddef;\n\n");
}

sub _convertOverstones {
    my ($my, $diagram) = @_;

    return unless (@{$diagram->getoverlist});

    my ($color, $number);
    my $text_x = 0;
    my $text_y = $my->{bottomLine} + 1;
    $text_y++ if ($my->{coords});
    for (my $ii = 0; $ii < @{$diagram->getoverlist}; $ii++) {
        my $int = $diagram->getoverlist->[$ii];
        $text_y += 1.2;# adjust for stone height
        my $x = $text_x;
        my $comma = 0;
        # all the overstones that were put on this understone:
        for (my $jj = 0; $jj < @{$int->{overstones}}; $jj += 2) {
            if ($comma ) {
                $my->print(', ');
            }
            $color = $int->{overstones}[$jj];
            local $my->{stoneOffset} = $my->{offset};   # turn off doubleDigits
            $number = $my->_checkStoneNumber($int->{overstones}[$jj+1]);
            # draw the overstone
            $my->_preamble();    # start another figure
            $my->mpprint("${color}stone(0, 0, \"$number\");\n");
            $my->mpprint("endfig;\n");
            $comma = 1;
        }
        # the 'at' stone
        if (exists($int->{black})) {
            $color = 'black';
        } elsif (exists($int->{white})) {
            $color = 'white';
        } else {
            carp("Oops: understone is not black or white? " .
                 "This isn't supposed to be possible!");
            next;
        }
        # at
        $my->print(' at ');
        # draw the at-stone
        $my->_preamble();    # start another figure
        if (exists($int->{number})) {
            $my->mpprint("${color}stone(0, 0, \"$int->{number}\");\n");
        } elsif (exists($int->{mark})) {
            $my->mpprint("triangle${color}stone(0, 0);\n");
        } else {
            my $mv = '';
            $mv .= " black node=$int->{black}" if (exists($int->{black}));
            $mv .= " white node=$int->{white}" if (exists($int->{black}));
            carp("Oops: understone$mv is not numbered or marked? " .
                 "This isn't supposed to be possible!");
        }
        $my->mpprint("endfig;\n");
        if ($ii < @{$diagram->getoverlist} - 1) {
            $my->print(";\\hfil\\break\n");
        }
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

# get tex for intersection hash from $diagram.
sub _convertIntersection {
    my ($my, $diagram, $x, $y) = @_;

    my $int = $diagram->get(&{$my->{diaCoords}}($x, $y));
    if (exists($int->{number})) {
        my $num = $my->_checkStoneNumber($int->{number}); # numbered stone
        if (exists($int->{black})) {
            $my->mpprint("blackstone($x, $y, \"$num\");\n");
        }elsif (exists($int->{white})) {
            $my->mpprint("whitestone($x, $y, \"$num\");\n");
        } else {
            carp("Can't number an empty intersection at $x, $y");
        }
    } elsif (exists($int->{mark})) {
        if (exists($int->{black})) {
            $my->mpprint("triangleblackstone($x, $y);\n");
        }elsif (exists($int->{white})) {
            $my->mpprint("trianglewhitestone($x, $y);\n");
        } else {
            carp("Can't triangle an empty intersection at $x, $y");
        }
    } elsif (exists($int->{label})) {
        my $label = $int->{label};
        if (exists($int->{black})) {
            $my->mpprint("blackstone($x, $y, \"$label\");\n");
        } elsif (exists($int->{white})) {
            $my->mpprint("whitestone($x, $y, \"$label\");\n");
        } else {
            $my->mpprint("labelintersection($x, $y, \"$label\");\n");
            return;
        }
    } elsif (exists($int->{black})) {
            $my->mpprint("blackstone($x, $y, \"\");\n");
    } elsif (exists($int->{white})) {
            $my->mpprint("whitestone($x, $y, \"\");\n");
    }
}

sub _preamble {
    my ($my) = @_;

    $my->{mpfignum} = 0 unless(exists($my->{mpfignum}));
    $my->{mpfignum}++;
    $my->print("\\epsffile{$my->{mpfigname}.$my->{mpfignum}}\n");
    $my->mpprint("beginfig($my->{mpfignum});\n");
}

sub _interlude {
    my ($my) = @_;

    # print coordinates along the bottom
    if ($my->{coords}) {
        $my->mpprint("% bottom coordinates:\n");
        my $y = $my->{bottomLine} + 1;
        for (my $x = $my->{leftLine}; $x <= $my->{rightLine}; $x++) {
            last if($x > 24);  # missing I, so only 25 letters in horz coords
            my $coord = substr("ABCDEFGHJKLMNOPQRSTUVWXYZ", ($x - 1), 1);
            $my->mpprint("label(\"$coord\", boardXY($x, $y)) withcolor black;\n");
        }
    }
}

sub _postamble {
    my ($my) = @_;

    if ($my->{twoColumn}) {
        $my->print("\n\n");
    } elsif ($my->{longComments}) {
        $my->print("\\par\\vfil\n",
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
        $my->print("\\par\\vfil\n",
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

Is this a trick question?

=head1 AUTHOR

Reid Augustin, E<lt>reid@hellosix.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut

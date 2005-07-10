# $Id: Diagram.pm 143 2005-06-03 21:05:57Z reid $

#   Diagram
#
#   Copyright (C) 2005 Reid Augustin reid@netchip.com
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

Games::Go::Diagram - Perl extension to make go diagrams similar to printed diagrams

=head1 SYNOPSIS

 use Games::Go::Diagram

 my $diagram = Games::Go::Diagram->new (options);
 $diagram->put($coords, 'white' | 'black', ? number ?);
 $diagram->mark($coords);
 $diagram->label($coords, 'a');
 $diagram->get($coords);
 my $new_diagram = $diagram->next;


=head1 DESCRIPTION

A Games::Go::Diagram object represents a diagram similar to those
seen in go textbooks and magazines.  The caller B<put>s 'white' or
'black' stones (possibly B<number>ed), on the intersection selected
by $coords.  The caller may also B<mark> and B<label> intersections
and stones.

B<put>ting, B<mark>ing, B<label>ing and B<property>ing are 'actions'.
Actions are provisional until the B<node> method is called.  If any
provisioanl actions cause a conflict, none of the actions associated
with the node are applied, and the B<node> method either calls a
user-defined callback function, or returns an error.

When a conflict occurs, the caller should dispose of the current
B<Diagram> by B<get>ting the information from each intersection and
doing something (like printing it).  Then the caller converts the
B<Diagram> to the starting point of the next diagram by calling the
B<clear> method.  Alternatively, the caller may save the current
B<Diagram> and create the starting point for the next diagram by
calling the B<next> method.  B<clear> and B<next> may also be called
at arbitrary times (for example, to launch a variation diagram).

'coords' may be any unique identifier for the intersection.  For
example:

    my $coords = 'qd';          # SGF format
    my $coords = 'a4';          # NNGS / IGS style coordinates
    my $coords = "$x,$y";       # real coordinates
    my $coords = 'George';      # as long as there's only one George

=cut

use strict;
require 5.001;

package Games::Go::Diagram;
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

# the following are valid options to ->new.  they are also preserved
#       or copied during calls to ->clear and ->next (except hoshi,
#       black, and white)
# watch out for reference copies!
our %options = (hoshi             => [],
                black             => [],
                white             => [],
                node              => 1,
                callback          => undef,
                enable_overstones => 1,
                overstone_eq_mark => 1);

######################################################
#
#       Public methods
#
#####################################################

=head1 METHODS

=over

=item my $diagram = Games::Go::Diagram-E<gt>B<new> (?options?)

A B<new> Games::Go::Diagram can take the following options:

Options:

=over

=item B<hoshi> =E<gt> ['coords', ...]

A reference to a list of coordinates where the Diagram should place
hoshi points.

=item B<black> =E<gt> ['coords', ...]

A reference to a list of coordinates where the Diagram should start
with black stones already in place.

=item B<white> =E<gt> ['coords', ...]

A reference to a list of coordinates where the Diagram should start
with white stones already in place.

=item B<callback> =E<gt> \&user_defined_callback

A reference to a user defined callback function.  The user callback
is called (with a reference to the B<Diagram> as the only argument)
when B<node> is called after conflict is detected.

The user callback should either save the current B<Diagram> and
call <next>, or flush the B<Diagram> (by printing for example) and
call <clear>.

If the user callback is defined, a call to B<node> always returns
non-zero (the current node number).

=item B<enable_overstones> =E<gt> true | false

If true (default), overstones are enabled and may be created by
the B<Diagram> during a call to the B<put> method.  The user must be
prepared to deal with overstones information in response to a
call to the B<get> method.

=item B<overstone_eq_mark> =E<gt> true | false

If true (default), overstones are assumed to be indistinguishable
from B<mark>s which means there can be conflicts between B<mark>s
and overstones.  If false, B<marks> and overstones are assumed
to use different symbols and there are no conflicts between them.

=back

=cut

sub new {
    my ($proto, %args) = @_;

    my $my = {};
    bless($my, ref($proto) || $proto);
    $my->{number} = {};
    $my->{property} = {};
    $my->{name} = [];
    $my->{offset} = 0;
    foreach (keys(%options)) {
        $my->{$_} = $options{$_};  # transfer default options
    }
    # transfer user args
    foreach (keys(%args)) {
        croak("I don't understand option $_\n") unless(exists($options{$_}));
        $my->{$_} = $args{$_};  # transfer user option
    }
    foreach my $type (qw(hoshi black white)) {
        next unless (exists($my->{$type}));
        my $coordList =  delete($my->{$type});
        for (my $ii = 0; $ii < @{$coordList}; $ii++) {
            $my->{board}{$coordList->[$ii]}{$type} = $my->{node};
            if (($type eq 'white') and
                exists($my->{board}{$coordList->[$ii]}{black})) {
                carp("Black and white on the same intersection (at $coordList->[$ii]");
            }
        }
    }
    $my->{actions} = [];        # no actions yet
    $my->{provisional} = 1;     # make all actions provisional
    return($my);
}

=item $diagram-E<gt>B<clear>

Clears the B<Diagram>.  All B<mark>s, B<label>s, and B<number>s are
removed from the stones and intersections.  All B<capture>d stones
are removed, and all overstones are deleted (at which point, the
B<Diagram> is in the same state as a B<new> B<Diagram>).  Pending
actions that were not applied due to conflicts are now applied to
the B<clear>ed B<Diagram>.

The following options are preserved:

=over

=item *

node    (gets incremented)

=item *

callback

=item *

enable_overstones

=item *

overstone_eq_mark

=back

Z<>

=cut

sub clear {
    my ($my) = @_;

# print "clear\n";
    my $actions = delete($my->{actions});       # save pending actions
    foreach my $key (keys(%{$my})) {
        next if (($key eq 'board') or
                 exists($options{$key}));
        delete($my->{$key});
    }
    my %new_board;
    # make a new board, keeping only hoshi and un-numbered stones
    foreach my $coords (keys(%{$my->{board}})) {
        my $int = $my->{board}{$coords};   # intersection
        $new_board{$coords}{hoshi} = $int->{hoshi} if(exists($int->{hoshi}));
        my $stone = $my->_game_stone_int($int);
        $new_board{$coords}{$stone} = 0 if (defined($stone));
    }
    delete($my->{board});
    $my->{board} = \%new_board;
    if (@{$actions}) {
        foreach(@{$actions}) {
            &{$_}($my);             # call the closures
        }
    }
    $my->{node}++;
    $my->{actions} = [];        # no actions yet
    $my->{provisional} = 1;     # make all actions provisional
    return $my;
}

=item my $nextDiagram = $diagram-E<gt>B<next>

Creates a new B<Diagram> object starting from the current
B<Diagram>.  $nextDiagram is the starting point for the next
B<Diagram> in a series, or for a variation.

As with the B<clear> method, all B<capture>d stones are removed, and
all overstones are deleted.  Pending actions that were not
applied due to conflicts are now applied to the B<next> B<Diagram>.

The following options are preserved:

=over

=item *

node    (gets incremented)

=item *

callback

=item *

enable_overstones

=item *

overstone_eq_mark

=back

Z<>

=cut

sub next {
    my ($my) = @_;

    my (@hoshi, @black, @white);
# print "next\n";
    foreach my $coords (keys(%{$my->{board}})) {
        my $int = $my->{board}{$coords};        # intersection
        if (exists($int->{hoshi})) {
            push(@hoshi, $coords);
        }
        my $stone = $my->_game_stone_int($int);
        next unless(defined($stone));
        push(@white, $coords) if($stone eq 'white');
        push(@black, $coords) if($stone eq 'black');
    }
    my %o;
    foreach my $key (keys(%options)) {
        next if (($key eq 'black') or
                 ($key eq 'white') or
                 ($key eq 'hoshi') or
                 ($key eq 'node'));
        $o{$key} = $my->{$key}; # watch out for reference copies!
    }
    my $next = Games::Go::Diagram->new(
        hoshi    => \@hoshi,
        white    => \@white,
        black    => \@black,
        %o,
        );
    foreach(@{$my->{actions}}) {
        &{$_}($next);           # call the closures on new Diagram
    }
    $next->node;                # and complete them
    return $next;
}

=item $diagram-E<gt>B<hoshi(@hoshi_coords)>

Adds the coords listed in @hoshi_coords to any existing hoshi
points.  In array context, returns the list of coords that are hoshi
points.  In scalar context, returns a reference to the list.

=cut

sub hoshi {
    my ($my, @new_hoshi) = @_;

    foreach(@new_hoshi) {
        $my->{board}{$_}{hoshi} = $my->{node};
    }
    my @hoshi;
    foreach (keys(%{$my->{board}})) {
        push (@hoshi, $_) if (exists($my->{board}{$_}{hoshi}));
    }
    return wantarray ? @hoshi : \@hoshi;
}

=item $diagram-E<gt>B<node>

All actions (B<put>, B<mark>, B<label> and B<property>) are
provisional until B<node> is called.  This makes a collection of
actions atomic.  A B<Diagram> node is analogous to a Smart Go Format
(SGF) node.  If there are no conflicts with the collected
provisional actions, B<node> incorporates them into the B<Diagram>
and returns non-zero (the current node number).

If there is a conflict and a user B<callback> is defined, B<node>
calls the callback with a reference to the B<Diagram> ($diagram) as
the only argument.  The user callback should either flush the
B<Diagram> and call B<clear> (to reuse the B<Diagram>) or save the
current B<Diagram>, and call B<next> (to generate a new B<Diagram>).

If there is a conflict and no user B<callback> is defined, B<node>
returns 0.  The user should either:

=over

=item *

flush the current B<Diagram> and call $diagram-E<gt>B<clear>
to continue working with the current B<Diagram>, or:

=item *

save the current B<Diagram> (and call $diagram-E<gt>B<next> to
create a new B<Diagram> to continue working with)

Z<>

=back

Calling either B<next> or B<clear> causes the pending collection of
conflicting actions to be incorporated into the resulting
B<Diagram>.

=cut

sub node {
    my ($my) = @_;

# print "node $my->{node}\n";
    if ($my->{conflict}) {
        if (exists($my->{callback})) {
# print "calling callback\n";
            &{$my->{callback}}($my);
            return $my->{node};
        }
        return 0;               # conflict: user needs to do something
    }
    $my->{provisional} = 0;     # make all actions actual
    foreach(@{$my->{actions}}) {
        &{$_}($my);             # call the closures
    }
    $my->{actions} = [];        # clear actions list
    $my->{provisional} = 1;     # make all actions provisional
    return ++$my->{node};
}

=item $diagram-E<gt>B<put> ('coords', 'black' | 'white',  ? number ? )

B<put> a black or white stone on the B<Diagram> at B<coords>.  The
stone color is must be 'black' or 'white' (case insensitive, 'b' and
'w' also work).  Optional B<number> is the number on the stone.  If
not defined, the stone is un-numbered (which is probably a mistake
except at the very start of a B<Diagram>.

B<put>ting can cause any of the following conflicts:

=over

=item *

stone is numbered and number is already used

=item *

stone is numbered and the intersection is already labeled

Z<>

=back

In certain situations, (notably ko and snapbacks but also some other
capturing situations), B<put> stones may become overstones.
overstones are stones played on an intersection that contains a
stone that has been B<capture>d, but not yet removed from the
B<Diagram>.  There are two kinds of overstones: normal and
B<mark>ed, depending on the state of the underlying (B<capture>d but
not yet removed) stone.

If the underlying stone is B<number>ed or B<label>ed, the overstone
is normal and there will be no conflicts (unless the number is
already used!).

If the underlying stone is un-B<number>ed and un-B<label>ed, the
B<Diagram> attempts to convert it into a B<mark>ed stone.  If the
conversion succeeds, the overstone becomes a marked overstone,
and there is no conflict.

The conversion of the underlying stone causes a conflict if:

=over

=item *

a stone of the same color as the underlying stone has already
been converted elsewhere in the B<Diagram>, or

=item *

B<overstone_eq_mark> is true and a mark of the same color as
the underlying stone exists elsewhere in the B<Diagram>.

=back

See the B<get> method for details of how overstone information is
returned.

=cut

sub put {
    my ($my, $coords, $color, $num) = @_;

    return 0 unless($my->_checkArgs('put', \$coords, \$color, \$num));
    my $num_msg = defined($num) ? " at move $num" : '';
    $my->{board}{$coords} = {} unless defined($my->{board}{$coords});
    my $int = $my->{board}{$coords};    # intersection
    if (exists($int->{$color}) and              # same color and
        ((not defined($num) and
          not exists($int->{number})) or        # both unnumbered or
         (defined($num) and
          exists($int->{number}) and            # both the same number
          ($num == $int->{number})))) {
        return $my->{node};                     # it's exactly the same
    }
    if (defined($my->game_stone($coords))) {    # must not be a stone here now
        my $err = "coords = $coords, new color = $color,\nalready here: ";
        $err .= $my->game_stone($coords) . ' ';
        if(exists($int->{overstones})) {
            my $ii = scalar(@{$int->{overstones}}) - 2; # get last two entries
            $err .= $int->{overstones}[$ii + 1];        # number of last stone played
        } elsif (exists $int->{number}) {
            $err .= $int->{number};
        } else {
            $err .= '(numberless)';
        }
        carp("can't 'put' a stone on top of a stone$num_msg: $err");
        return 0;
    }
    if ($my->{provisional}) {
        push (@{$my->{actions}}, sub { $_[0]->put($coords, $color, $num); } );
    }
    my $makeOverStone = (exists($int->{white}) or       # stone already here?
                         exists($int->{black}));        #   make an overstone
    if ($makeOverStone) {
        $my->_overstone($coords, $color, $num)
    } elsif (exists($int->{mark}) or            # not supposed to be marks on empty anyway
             (defined($num) and                 # new stone is numbered?
              (exists($my->{number}{$num}) or   # already used number?
               exists($int->{label})))) {       # label already here?
# print "put conflict $color$num_msg\n";
        $my->{conflict} = 1;
        return 0;
    }
    unless ($my->{provisional}) {
        if (defined($num)) {
            $int->{number} = $num unless($makeOverStone);
            $my->{number}{$num} = $my->{node};
        }
        delete($int->{capture});
        $int->{$color} = $my->{node} unless ($makeOverStone);
    }
    return $my->{node};
}

sub _overstone {
    my ($my, $coords, $color, $num) = @_;

    unless(defined($num)) {     # overstones must be numbered
# print "overstone 0 conflict $color\n";
        $my->{conflict} = 1;
        return 0;
    }
    my $int = $my->{board}{$coords};
    my $underColor = exists($int->{black}) ? 'black' : 'white';
    if (exists($int->{number}) or
        exists($int->{label})) {
        # OK, no conflict
    } else {
        # understone isn't numbered/labeled,
        # can we convert it to a marked stone?
        if (exists($my->{marked_overstone}{$underColor})) {
            if ($my->{marked_overstone}{$underColor} ne $coords) {
# print "overstone 1 conflict $color $num\n";
                $my->{conflict} = 1;
                return 0;
            } # else this intersection is already marked
        } elsif ($my->{overstone_eq_mark} and
                 exists($my->{mark}{$underColor})) {
# print "overstone 2 conflict $color $num\n";
            $my->{conflict} = 1;
            return 0;
        }
        unless($my->{provisional}) {
            if ($my->{overstone_eq_mark}) {
                $int->{mark} = $my->{node};
            } else {
                $int->{overstone} = $my->{node};
            }
            $my->{marked_overstone}{$underColor} = $coords;
            $my->{mark}{$underColor} = $my->{node} if ($my->{overstone_eq_mark});
        }
    }
    unless($my->{provisional}) {
        push(@{$my->{overlist}}, $int) unless(exists($int->{overstones}));
        push(@{$int->{overstones}}, $color, $num);
        delete($int->{capture});
    }
}

=item $diagram-E<gt>B<renumber>($coords, $color, $old_num, $new_num);

Changes the number of a stone already on the board.  $color, and
$old_num must match the existing color and number for the stone at
$coords ($old_num or $new_num may be undef for an un-numbered
stone).  Only the displayed stone is compared for the match,
overstones (B<game_stone>s) are not considered.

Fails and returns 0 if:

=over

=item *

there is no diagram stone on the intersection, or

=item *

$color or $old_num don't match, or

=item *

$new_num is already used, or

=item *

a B<property> item exists for $old_num and $new_num is undef

=back

If none of the above, B<renumber> sets the new number and returns 1.

=cut

sub renumber {
    my ($my, $coords, $color, $old_num, $new_num) = @_;

    return 0 unless($my->_checkArgs('renumber', \$coords, \$color, \$old_num));
    return 0 unless($my->_checkArgs('renumber', \$coords, \$color, \$new_num));
    $my->{board}{$coords} = {} unless defined($my->{board}{$coords});
    my $int = $my->{board}{$coords};    # intersection
    return 0 if (not exists($int->{$color}) or
                 (defined($new_num) and
                  exists($my->{number}{$new_num})) or
                 (defined($old_num) and
                  exists($my->{property}{$old_num}) and
                  not defined ($new_num)));
    return 0 unless((not defined($old_num) and
                     not exists($int->{number})) or
                    (defined($old_num) and
                     defined($int->{number}) and
                     ($old_num == $int->{number})));
    delete($my->{number}{$old_num}) if(defined($old_num));
    if (defined($new_num)) {
        $int->{number} = $new_num;
        $my->{number}{$new_num} = $my->{node};
        if(defined($old_num) and
           exists($my->{property}{$old_num})) {
            $my->{property}{$new_num} = delete($my->{property}{$old_num});
        }
    } else {
        delete($int->{number});
    }
    return 1;
}

=item my $offset = $diagram-E<gt>B<offset>($new_offset);

Set a new offset for the diagram if $new_offset is defined.  Returns
the current value of the offset, or 0 if no offset has been set.

Note that B<Diagram> doesn't use the offset for anything, but
external programs (like a converter) can use it to adjust the
numbering.

=cut

sub offset {
    my ($my, $new_offset) = @_;

    $my->{offset} = $new_offset if(defined($new_offset));
    return $my->{offset};
}


sub _checkArgs {
    my ($my, $name, $coords, $color, $num) = @_;

    my $num_msg = defined($$num) ? " at move $$num" : '';
    unless(defined($$coords)) {
        carp("'$name' expects a '\$coords' argument$num_msg");
        return 0;
    }
    my $c = $$color;
    $c = 'undef' unless defined($c);
    $c = lc $c;
    $c = 'black' if ($c eq 'b');
    $c = 'white' if ($c eq 'w');
    if (($c ne 'white') and
        ($c ne 'black')) {
        carp("'$name' expects 'white' or 'black', not $$color$num_msg");
        return 0;
    }
    if (defined($$num) and
        ($$num =~ /\D/)) {
        carp("'$name' expects number or undef for $$color stone, not $$num$num_msg");
        return 0;
    }
    $$color = $c;       # normalize color
    return 1;
}

=item $diagram-E<gt>B<label>('coords', 'char');

Place a label on an intersection.  B<char> must be a single letter
from A to Z or from a to z.

The same label can be applied to several intersections only if they
are all labeled within a single B<node>.

If the intersection or stone is already labeled, or occupied by a
marked, or numbered stone, or if the label has already been used
outside the labeling group, B<label> causes a conflict.

=cut

sub label {
    my ($my, $coords, $label) = @_;

    unless(defined($coords)) {
        carp("'label' expects a '\$coords' argument");
        return 0;
    }
    unless (defined($label) and
        ($label =~ /^[[:upper:][:lower:]]$/)) {
        $label = 'undef' unless defined($label);
        carp("'label' expects a single letter, not $label");
        return 0;
    }
    if ($my->{provisional}) {
# print "provisional ";
        push (@{$my->{actions}}, sub { $_[0]->label($coords, $label); } );
    }
# print "label $coords with $label\n";
    $my->{board}{$coords} = {} unless defined($my->{board}{$coords});
    my $int = $my->{board}{$coords};    # intersection
    if ((exists($int->{label}) and
         ($int->{label} ne $label)) or  # different label already here?
        exists($int->{mark}) or         # a mark?
        exists($int->{number}) or       # a number?
        (exists($my->{label}{$label}) and               # label already used?
         ($my->{label}{$label} != $my->{node}))) {      # outside labeling group?
# print "label conflict $coords $label\n";
        $my->{conflict} = 1;
        return 0;
    }
    unless ($my->{provisional}) {
        $int->{label} = $label;
        $int->{$label} = $my->{node};
        $my->{label}{$label} = $my->{node};
    }
    return $my->{node};
}

=item $diagram-E<gt>B<mark>('coords');

Place a mark on a stone (empty intersections cannot be marked).

The mark can be applied to several stones only if they are either: 

=over

=item *

different color stones (black, white)

=item *

all marked within one B<node>.

=back

Z<>

The B<mark> causes a conflict if:

=over

=item *

the stone is a B<label>led or numbered stone, or

=item *

the same color mark already exists in the B<Diagram> for a
different B<mark>ing group, or

=item *

B<overstone_eq_mark> is true and there is already an
overstone of the same color in the B<Diagram>.

=back

Z<>

=cut

sub mark {
    my ($my, $coords) = @_;

    unless(defined($coords)) {
        carp("'mark' expects a '\$coords' argument");
        return 0;
    }
    if ($my->{provisional}) {
        push (@{$my->{actions}}, sub { $_[0]->mark($coords); } );
    }
    $my->{board}{$coords} = {} unless defined($my->{board}{$coords});
    my $int = $my->{board}{$coords};    # intersection
    my $color = 'empty';
    $color = 'white' if (exists($int->{white}));
    $color = 'black' if (exists($int->{black}));
    if (exists($int->{label}) or        # label already here?
        exists($int->{number}) or       # number already here?

        (exists($my->{mark}{$color}) and                # mark/color already used?
         ($my->{mark}{$color} != $my->{node}))) {       # outside group?
# print "put mark $coords\n";
        $my->{conflict} = 1;
        return 0;
    }
    unless ($my->{provisional}) {
        $int->{mark} = $my->{node};
        $my->{mark}{$color} = $my->{node};
    }
    return $my->{node};
}

=item my $nameListRef = $diagram-E<gt>B<name> (? name, ... ?)

Adds B<name>(s) to the current B<Diagram>.  Names accumulate by
getting pushed onto a list.

In array context, B<name> returns the current name list.  In scalar
context, B<name> returns a reference to the list of names.

=cut

sub name {
    my ($my, @names) = @_;

    if (defined($names[0])) {
        push (@{$my->{name}}, @names);
    }
    return wantarray ? @{$my->{name}} : $my->{name};
}

=item $diagram-E<gt>B<property> ($number, $propName, $propValue, ? $propValue... ?);

=item my $prop_ref = $diagram-E<gt>B<property> ($number);

=item my $all_props_ref = $diagram-E<gt>B<property> ();

If $propName and $propVal are defined, pushes them onto the
collection of properties associated with move $number.

Note that B<renumber>ing a move also B<renumber>s the properties.

If $number is defined and $propName/$propValue are not, B<property>
returns a reference to the (possibly empty) hash of property IDs and
property Values associated with the move number:

    my $prop_value = $prop_ref->{$propID}->[$idx].

If $number is not defined, returns a reference to the (possibly
empty) hash of properties stored in the B<Diagram>.  Hash keys are
the move number, and each hash value is in turn a hash.  The keys of
the property hashes are (short) property IDs and the hash values are
lists of property values for each property ID:

    my $prop_value = $all_props_ref->{$moveNumber}->{$propID}->[$idx]

B<property> (when $propName and $propValue are defined) is an action
(it is provisional until B<node> is called) because properties are
associated with a node in the SGF.  However, B<property> never
causes a conflict.

=cut

sub property {
    my ($my, $number, $propId, @propVals) = @_;

    if (defined($propId)) {
        push(@{$my->{actions}}, sub { push(@{$_[0]->{property}{$number}{$propId}}, @propVals); } );
    }
    return {} unless(exists($my->{property}));
    return ($my->{property}{$number} || {}) if (defined($number));
    return $my->{property};
}

=item $diagram-E<gt>B<capture> ('coords')

Captures the stone at the intersection.

Note that B<capture> has no visible affect on the diagram.  Rather,
it marks the stone so that it is removed when creating the B<next>
B<Diagram>.

B<capture> is not considered an action because it cannot cause a
conflict or change the visible status of the board.

=cut

sub capture {
    my ($my, $coords) = @_;

    unless(defined($coords)) {
        carp("'capture' expects a '\$coords' argument");
        return 0;
    }
    my $stone = $my->game_stone($coords);
    unless (defined($stone)) {
        carp("'capture(\$coords=$coords)' called, but there's no stone here");
        return undef;
    }
    my $int = $my->{board}{$coords};    # intersection
    $int->{capture} = $my->{node};
    return $my->{node};
}

=item $diagram-E<gt>B<remove> ('coords')

Removes the stone at the intersection.

Unlike B<capture>, B<remove> changes the visible status of the
B<Diagram>: the stone is deleted, along with all marks and letters
(only the 'hoshi', if any, is retained).

B<remove> is typically used at the start of a variation to remove
any stones that are improperly placed for the variation.  It is
closely related to the AddEmpty (AE) SGF property.

=cut

sub remove {
    my ($my, $coords) = @_;

    unless(defined($coords)) {
        carp("'remove' expects a '\$coords' argument");
        return 0;
    }
    my $int = $my->{board}{$coords};    # intersection
    foreach (keys(%{$int})) {
        delete($int->{$_}) unless($_ eq 'hoshi');
    }
    return $my->{node};
}

=item my $stone = $diagram-E<gt>B<game_stone>(coords);

Returns 'black' or 'white' if there is a stone currently on the
intersection, otherwise returns undef.

Note that the return value is determined by the game perspective,
not the diagram perspective.  If a stone is B<put> and later
B<capture>d, B<game_stone> returns undef even though the diagram
should still show the original stone.  If a white stone is B<put>
and later B<capture>d, and then a black stone is B<put>,
B<game_stone> returns 'black', and B<get> indicates that a white
stone should be displayed on the diagram.

Note also that since B<put> is provisional until B<node> is called.
If you use B<game_stone> to check for liberties and captures, it
must be done I<after> the call to B<node> that realizes the B<put>.

=cut

sub game_stone {
    my ($my, $coords) = @_;

    unless(defined($coords)) {
        carp("'game_stone' expects a '\$coords' argument");
        return 0;
    }
    my $int = $my->{board}{$coords};    # intersection
    return $my->_game_stone_int($my->{board}{$coords});
}

sub _game_stone_int {
    my ($my, $int) = @_;

    return undef unless(defined($int));
    return undef if(exists($int->{capture}));          # well, it *was* here a moment ago...
    # check overstone history
    if(exists($int->{overstones})) {
        my $ii = scalar(@{$int->{overstones}}) - 2;     # get last two entries
        return($int->{overstones}[$ii]);                # last color played
    }
    return 'black' if (exists($int->{black}));
    return 'white' if (exists($int->{white}));
    return undef;
}

=item $diagram-E<gt>B<get> ('coords')

Return the current status of the intersection.  Status is returned
as a reference to a hash.  The keys of the hash indicate the items
of interest, and the values of the hash are the indices where the
item was applied, except where noted below.

Only keys that have been applied are returned - an empty hash means
an empty intersection.

The hash keys can be any of:

=over

=item 'hoshi'

This intersection is a hoshi point.  Note that since hoshi points
are determined at B<new> time, the value of this hash entry is
always 0.  This key is returned even if a stone has been placed on
the intersection.

=item 'white'

The color of a stone at this intersection.

=item 'black'

The color of a stone at this intersection.

=item 'number'

The hash value is the number on the stone.  The node for
B<number> is found in the 'black' or 'white' hash value.

=item 'capture'

The stone on this intersection has been B<capture>d, the
intersection is currently empty from the perspective of the game.

=item 'mark'

The intersection or stone is marked.

=item 'overstone'

If B<overstone_eq_mark> is false, this hash entry gets the current
node when an un-numbered, un-labeled stone is converted to an
overstone.  If B<overstone_eq_mark> is false, this hash entry is
never set (under the same circumstances, the intersection is
'mark'ed instead).

=item 'label'

The hash key is the word 'label', and the value is the single
character passed to the B<label> method.  Note that the node can be
retrieved with:

    my $intersection = $diagram->get;
    my $label = $intersection->{label};
    my $label_node = $intersection->{$label};

=item label

The hash key is the single character a-z or A-Z that is the B<label>
for this intersection or stone.  The hash value is the node.

=item 'overstones'

If this hash entry exists it means that one or more stones were
overlayed on the stone that is currently displayed on this
intersection of the B<Diagram>.

The hash value is a reference to an array of color/number pairs.
The colors and numbers were passed to the B<put> method which
decided to convert the stone into an overstone.

This is typically seen as notes to the side of the diagram saying
something like "black 33 was played at the marked white stone".  In
this example. the information returned by B<get> describes 'the
marked white stone', while 'black' will be the first item in the
'overstones' array, and '33' will be the second.

=back

The hash reference returned by B<get> points to the data in the
B<Diagram> object - don't change it unless you know what you are
doing.

=cut

sub get {
    my ($my, $coords) = @_;

    unless(defined($coords)) {
        carp("'get' expects a '\$coords' argument");
        return {};
    }
    return $my->{board}{$coords} || {};
}

=item my $first_number = $diagram-E<gt>B<first_number>

Returns the lowest number B<put> on the B<Diagram>, or 0 if no
numbered stones have been B<put>.

=cut

sub first_number {
    my ($my) = @_;

    my $first;
    foreach my $num (keys(%{$my->{number}})) {
        $first = $num unless(defined($first));
        $first = $num if ($num < $first);
    }
    $first = 0 unless(defined($first));
    return $first;
}

=item my $last_number = $diagram-E<gt>B<last_number>

Returns the highest number B<put> on the B<Diagram>, or 0 if no
numbered stones have been B<put>.

=cut

sub last_number {
    my ($my) = @_;

    my $last = 0;
    foreach my $num (keys(%{$my->{number}})) {
        $last = $num if ($num > $last);
    }
    return $last;
}

=item my $parentDiagram = $diagram-E<gt>B<parent> (? $parent ?)

If $parent is defined, sets the B<parent> for this diagram.

Always returns the current value of B<parent> (possibly undef).

=cut

sub parent {
    my ($my, $new) = @_;

    $my->{parent} = $new if (defined($new));
    return $my->{parent};
}

=item my $move_number = $diagram-E<gt>B<var_on_move> (? $new_number ?)

If $new_number is defined, sets the B<var_on_move> for this diagram.
This is intended to be used in conjunction with the <Bparent>
information to title diagrams such as 

    my $title = "Variation 2 on move " .
                $diagram->var_on_move .
                " in " .
                $diagram->parent->name;

Always returns the current value of B<var_on_move> (possibly undef).

=cut

sub var_on_move {
    my ($my, $new) = @_;

    $my->{var_on_move} = $new if (defined($new));
    return $my->{var_on_move};
}

=item my $overListRef = $diagram-E<gt>B<getoverlist>

Returns a reference to the list of intersections with overstones.
The list members are the same intersection hash references returned
by the B<get> method.

The list is sorted by the order the intersections first had an
overstone B<put> on.  If there are no intersections with overstones,
returns a reference to an empty list.

=cut

sub getoverlist {
    my ($my) = @_;

    return($my->{overlist}) if (exists($my->{overlist}));
    return [];
}

=item my $user = $diagram-E<gt>B<user> ( ? $new_user ? )

If $new_user is defined, sets the B<user> value for the B<Diagram>.
Note that the B<user> is not used within B<Diagram>, but can be used
by external code for any purpose.  Most useful is probably if
$new_user is a reference to a hash of user-defined items of
interest.

Returns the current B<user> value (default is undef).

=cut

sub user {
    my ($my, $user) = @_;

    $my->{user} = $user if(defined($user));
    return $my->{user};
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

With the current architecture, conflicts within a node are not
detected.  I think this would probably be malformed SGF.  This
deficiency could be fixed by adding a 'shadow' diagram to which
provisional actions are applied.

=head1 AUTHOR

Reid Augustin, E<lt>reid@netchip.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Reid Augustin

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut


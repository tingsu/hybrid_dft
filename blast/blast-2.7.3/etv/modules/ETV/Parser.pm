####################################################################
#
#    This file was generated using Parse::Yapp version 1.05.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package ETV::Parser;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
#Included Parse/Yapp/Driver.pm file----------------------------------------
{
#
# Module Parse::Yapp::Driver
#
# This module is part of the Parse::Yapp package available on your
# nearest CPAN
#
# Any use of this module in a standalone parser make the included
# text under the same copyright as the Parse::Yapp module itself.
#
# This notice should remain unchanged.
#
# (c) Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (see the pod text in Parse::Yapp module for use and distribution rights)
#

package Parse::Yapp::Driver;

require 5.004;

use strict;

use vars qw ( $VERSION $COMPATIBLE $FILENAME );

$VERSION = '1.05';
$COMPATIBLE = '0.07';
$FILENAME=__FILE__;

use Carp;

#Known parameters, all starting with YY (leading YY will be discarded)
my(%params)=(YYLEX => 'CODE', 'YYERROR' => 'CODE', YYVERSION => '',
			 YYRULES => 'ARRAY', YYSTATES => 'ARRAY', YYDEBUG => '');
#Mandatory parameters
my(@params)=('LEX','RULES','STATES');

sub new {
    my($class)=shift;
	my($errst,$nberr,$token,$value,$check,$dotpos);
    my($self)={ ERROR => \&_Error,
				ERRST => \$errst,
                NBERR => \$nberr,
				TOKEN => \$token,
				VALUE => \$value,
				DOTPOS => \$dotpos,
				STACK => [],
				DEBUG => 0,
				CHECK => \$check };

	_CheckParams( [], \%params, \@_, $self );

		exists($$self{VERSION})
	and	$$self{VERSION} < $COMPATIBLE
	and	croak "Yapp driver version $VERSION ".
			  "incompatible with version $$self{VERSION}:\n".
			  "Please recompile parser module.";

        ref($class)
    and $class=ref($class);

    bless($self,$class);
}

sub YYParse {
    my($self)=shift;
    my($retval);

	_CheckParams( \@params, \%params, \@_, $self );

	if($$self{DEBUG}) {
		_DBLoad();
		$retval = eval '$self->_DBParse()';#Do not create stab entry on compile
        $@ and die $@;
	}
	else {
		$retval = $self->_Parse();
	}
    $retval
}

sub YYData {
	my($self)=shift;

		exists($$self{USER})
	or	$$self{USER}={};

	$$self{USER};
	
}

sub YYErrok {
	my($self)=shift;

	${$$self{ERRST}}=0;
    undef;
}

sub YYNberr {
	my($self)=shift;

	${$$self{NBERR}};
}

sub YYRecovering {
	my($self)=shift;

	${$$self{ERRST}} != 0;
}

sub YYAbort {
	my($self)=shift;

	${$$self{CHECK}}='ABORT';
    undef;
}

sub YYAccept {
	my($self)=shift;

	${$$self{CHECK}}='ACCEPT';
    undef;
}

sub YYError {
	my($self)=shift;

	${$$self{CHECK}}='ERROR';
    undef;
}

sub YYSemval {
	my($self)=shift;
	my($index)= $_[0] - ${$$self{DOTPOS}} - 1;

		$index < 0
	and	-$index <= @{$$self{STACK}}
	and	return $$self{STACK}[$index][1];

	undef;	#Invalid index
}

sub YYCurtok {
	my($self)=shift;

        @_
    and ${$$self{TOKEN}}=$_[0];
    ${$$self{TOKEN}};
}

sub YYCurval {
	my($self)=shift;

        @_
    and ${$$self{VALUE}}=$_[0];
    ${$$self{VALUE}};
}

sub YYExpect {
    my($self)=shift;

    keys %{$self->{STATES}[$self->{STACK}[-1][0]]{ACTIONS}}
}

sub YYLexer {
    my($self)=shift;

	$$self{LEX};
}


#################
# Private stuff #
#################


sub _CheckParams {
	my($mandatory,$checklist,$inarray,$outhash)=@_;
	my($prm,$value);
	my($prmlst)={};

	while(($prm,$value)=splice(@$inarray,0,2)) {
        $prm=uc($prm);
			exists($$checklist{$prm})
		or	croak("Unknow parameter '$prm'");
			ref($value) eq $$checklist{$prm}
		or	croak("Invalid value for parameter '$prm'");
        $prm=unpack('@2A*',$prm);
		$$outhash{$prm}=$value;
	}
	for (@$mandatory) {
			exists($$outhash{$_})
		or	croak("Missing mandatory parameter '".lc($_)."'");
	}
}

sub _Error {
	print "Parse error.\n";
}

sub _DBLoad {
	{
		no strict 'refs';

			exists(${__PACKAGE__.'::'}{_DBParse})#Already loaded ?
		and	return;
	}
	my($fname)=__FILE__;
	my(@drv);
	open(DRV,"<$fname") or die "Report this as a BUG: Cannot open $fname";
	while(<DRV>) {
                	/^\s*sub\s+_Parse\s*{\s*$/ .. /^\s*}\s*#\s*_Parse\s*$/
        	and     do {
                	s/^#DBG>//;
                	push(@drv,$_);
        	}
	}
	close(DRV);

	$drv[0]=~s/_P/_DBP/;
	eval join('',@drv);
}

#Note that for loading debugging version of the driver,
#this file will be parsed from 'sub _Parse' up to '}#_Parse' inclusive.
#So, DO NOT remove comment at end of sub !!!
sub _Parse {
    my($self)=shift;

	my($rules,$states,$lex,$error)
     = @$self{ 'RULES', 'STATES', 'LEX', 'ERROR' };
	my($errstatus,$nberror,$token,$value,$stack,$check,$dotpos)
     = @$self{ 'ERRST', 'NBERR', 'TOKEN', 'VALUE', 'STACK', 'CHECK', 'DOTPOS' };

#DBG>	my($debug)=$$self{DEBUG};
#DBG>	my($dbgerror)=0;

#DBG>	my($ShowCurToken) = sub {
#DBG>		my($tok)='>';
#DBG>		for (split('',$$token)) {
#DBG>			$tok.=		(ord($_) < 32 or ord($_) > 126)
#DBG>					?	sprintf('<%02X>',ord($_))
#DBG>					:	$_;
#DBG>		}
#DBG>		$tok.='<';
#DBG>	};

	$$errstatus=0;
	$$nberror=0;
	($$token,$$value)=(undef,undef);
	@$stack=( [ 0, undef ] );
	$$check='';

    while(1) {
        my($actions,$act,$stateno);

        $stateno=$$stack[-1][0];
        $actions=$$states[$stateno];

#DBG>	print STDERR ('-' x 40),"\n";
#DBG>		$debug & 0x2
#DBG>	and	print STDERR "In state $stateno:\n";
#DBG>		$debug & 0x08
#DBG>	and	print STDERR "Stack:[".
#DBG>					 join(',',map { $$_[0] } @$stack).
#DBG>					 "]\n";


        if  (exists($$actions{ACTIONS})) {

				defined($$token)
            or	do {
				($$token,$$value)=&$lex($self);
#DBG>				$debug & 0x01
#DBG>			and	print STDERR "Need token. Got ".&$ShowCurToken."\n";
			};

            $act=   exists($$actions{ACTIONS}{$$token})
                    ?   $$actions{ACTIONS}{$$token}
                    :   exists($$actions{DEFAULT})
                        ?   $$actions{DEFAULT}
                        :   undef;
        }
        else {
            $act=$$actions{DEFAULT};
#DBG>			$debug & 0x01
#DBG>		and	print STDERR "Don't need token.\n";
        }

            defined($act)
        and do {

                $act > 0
            and do {        #shift

#DBG>				$debug & 0x04
#DBG>			and	print STDERR "Shift and go to state $act.\n";

					$$errstatus
				and	do {
					--$$errstatus;

#DBG>					$debug & 0x10
#DBG>				and	$dbgerror
#DBG>				and	$$errstatus == 0
#DBG>				and	do {
#DBG>					print STDERR "**End of Error recovery.\n";
#DBG>					$dbgerror=0;
#DBG>				};
				};


                push(@$stack,[ $act, $$value ]);

					$$token ne ''	#Don't eat the eof
				and	$$token=$$value=undef;
                next;
            };

            #reduce
            my($lhs,$len,$code,@sempar,$semval);
            ($lhs,$len,$code)=@{$$rules[-$act]};

#DBG>			$debug & 0x04
#DBG>		and	$act
#DBG>		and	print STDERR "Reduce using rule ".-$act." ($lhs,$len): ";

                $act
            or  $self->YYAccept();

            $$dotpos=$len;

                unpack('A1',$lhs) eq '@'    #In line rule
            and do {
                    $lhs =~ /^\@[0-9]+\-([0-9]+)$/
                or  die "In line rule name '$lhs' ill formed: ".
                        "report it as a BUG.\n";
                $$dotpos = $1;
            };

            @sempar =       $$dotpos
                        ?   map { $$_[1] } @$stack[ -$$dotpos .. -1 ]
                        :   ();

            $semval = $code ? &$code( $self, @sempar )
                            : @sempar ? $sempar[0] : undef;

            splice(@$stack,-$len,$len);

                $$check eq 'ACCEPT'
            and do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Accept.\n";

				return($semval);
			};

                $$check eq 'ABORT'
            and	do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Abort.\n";

				return(undef);

			};

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Back to state $$stack[-1][0], then ";

                $$check eq 'ERROR'
            or  do {
#DBG>				$debug & 0x04
#DBG>			and	print STDERR 
#DBG>				    "go to state $$states[$$stack[-1][0]]{GOTOS}{$lhs}.\n";

#DBG>				$debug & 0x10
#DBG>			and	$dbgerror
#DBG>			and	$$errstatus == 0
#DBG>			and	do {
#DBG>				print STDERR "**End of Error recovery.\n";
#DBG>				$dbgerror=0;
#DBG>			};

			    push(@$stack,
                     [ $$states[$$stack[-1][0]]{GOTOS}{$lhs}, $semval ]);
                $$check='';
                next;
            };

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Forced Error recovery.\n";

            $$check='';

        };

        #Error
            $$errstatus
        or   do {

            $$errstatus = 1;
            &$error($self);
                $$errstatus # if 0, then YYErrok has been called
            or  next;       # so continue parsing

#DBG>			$debug & 0x10
#DBG>		and	do {
#DBG>			print STDERR "**Entering Error recovery.\n";
#DBG>			++$dbgerror;
#DBG>		};

            ++$$nberror;

        };

			$$errstatus == 3	#The next token is not valid: discard it
		and	do {
				$$token eq ''	# End of input: no hope
			and	do {
#DBG>				$debug & 0x10
#DBG>			and	print STDERR "**At eof: aborting.\n";
				return(undef);
			};

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Dicard invalid token ".&$ShowCurToken.".\n";

			$$token=$$value=undef;
		};

        $$errstatus=3;

		while(	  @$stack
			  and (		not exists($$states[$$stack[-1][0]]{ACTIONS})
			        or  not exists($$states[$$stack[-1][0]]{ACTIONS}{error})
					or	$$states[$$stack[-1][0]]{ACTIONS}{error} <= 0)) {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Pop state $$stack[-1][0].\n";

			pop(@$stack);
		}

			@$stack
		or	do {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**No state left on stack: aborting.\n";

			return(undef);
		};

		#shift the error token

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Shift \$error token and go to state ".
#DBG>						 $$states[$$stack[-1][0]]{ACTIONS}{error}.
#DBG>						 ".\n";

		push(@$stack, [ $$states[$$stack[-1][0]]{ACTIONS}{error}, undef ]);

    }

    #never reached
	croak("Error in driver logic. Please, report it as a BUG");

}#_Parse
#DO NOT remove comment

1;

}
#End of include--------------------------------------------------


#line 18 "shared/perl/modules/ETV/Parser.ym"

use English;


sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.05',
                                  yystates =>
[
	{#State 0
		DEFAULT => -1,
		GOTOS => {
			'input' => 1
		}
	},
	{#State 1
		ACTIONS => {
			'' => 2,
			'LINE' => 5
		},
		DEFAULT => -5,
		GOTOS => {
			'str' => 3,
			'node' => 4,
			'line' => 6
		}
	},
	{#State 2
		DEFAULT => 0
	},
	{#State 3
		DEFAULT => -2
	},
	{#State 4
		ACTIONS => {
			"\n" => 7
		}
	},
	{#State 5
		DEFAULT => -6
	},
	{#State 6
		ACTIONS => {
			'FILE' => 8
		},
		DEFAULT => -7,
		GOTOS => {
			'file' => 9
		}
	},
	{#State 7
		DEFAULT => -3
	},
	{#State 8
		DEFAULT => -8
	},
	{#State 9
		ACTIONS => {
			'TYPE' => 10
		},
		DEFAULT => -9,
		GOTOS => {
			'type' => 11
		}
	},
	{#State 10
		DEFAULT => -10
	},
	{#State 11
		ACTIONS => {
			'KIND' => 13
		},
		DEFAULT => -11,
		GOTOS => {
			'kind' => 12
		}
	},
	{#State 12
		ACTIONS => {
			'SKIP_REASON' => 14
		},
		DEFAULT => -13,
		GOTOS => {
			'skip_reason' => 15
		}
	},
	{#State 13
		DEFAULT => -12
	},
	{#State 14
		DEFAULT => -14
	},
	{#State 15
		ACTIONS => {
			'ARG_NAME' => 19
		},
		DEFAULT => -15,
		GOTOS => {
			'formal_arg_names_list' => 16,
			'formal_arg_names' => 18,
			'arg_name' => 17
		}
	},
	{#State 16
		ACTIONS => {
			'TEXT' => 20
		},
		DEFAULT => -20,
		GOTOS => {
			'text' => 21
		}
	},
	{#State 17
		DEFAULT => -18
	},
	{#State 18
		ACTIONS => {
			'ARG_NAME' => 19
		},
		DEFAULT => -16,
		GOTOS => {
			'arg_name' => 22
		}
	},
	{#State 19
		DEFAULT => -19
	},
	{#State 20
		DEFAULT => -21
	},
	{#State 21
		DEFAULT => -4
	},
	{#State 22
		DEFAULT => -17
	}
],
                                  yyrules  =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 'input', 0, undef
	],
	[#Rule 2
		 'input', 2,
sub
#line 25 "shared/perl/modules/ETV/Parser.ym"
{
  my $parser = $ARG[0];

  if ($parser->YYData->{PARENTS})
  {
    return ${$parser->YYData->{PARENTS}}[0];
  }
}
	],
	[#Rule 3
		 'str', 2,
sub
#line 34 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 4
		 'node', 7,
sub
#line 37 "shared/perl/modules/ETV/Parser.ym"
{
  my $parser = $ARG[0];

  # Change a currently processed file if a new path is specified.
  $parser->YYData->{FILE} = $ARG[2] if ($ARG[2]);

  my %node = (
    'line' => $ARG[1]
    , 'file' => $parser->YYData->{FILE}
    , 'type' => $ARG[3]
    , 'kind' => $ARG[4]
    , 'skip_reason' => $ARG[5]
    , 'formal_arg_names' => $ARG[6]
    , 'text' => $ARG[7]);

  # Do nothing when we encounter a line consisting just of whitespaces.
  return undef
    if (!$node{'line'} and !$node{'file'} and !$node{'type'} and !$node{'kind'} and !$node{'skip_reason'} and !$node{'foramal_arg_names'} and !$node{'text'});

  # Kind attribute can be specified just for CALL type.
  if ($node{'kind'} and (!$node{'type'} or $node{'type'} ne 'CALL'))
  {
    $parser->YYData->{ERRMSG} = "Node kind '$node{kind}' is specified but node type isn't 'CALL' (it's '$node{type}')";
    $parser->YYError;
    return undef;
  }

  # Create artificial root tree node to keep first-level children.
  if (!$parser->YYData->{PARENTS})
  {
    my $root = {
      'line' => undef
      , 'file' => undef
      , 'type' => 'ROOT'
      , 'kind' => undef
      , 'skip_reason' => undef
      , 'formal_arg_names' => undef
      , 'text' => undef};

    push(@{$parser->YYData->{PARENTS}}, $root);
  }

  # Update children for a current parent.
  if ($parser->YYData->{PARENTS} and scalar(@{$parser->YYData->{PARENTS}}))
  {
    push(@{${$parser->YYData->{PARENTS}}[-1]->{'children'}}, \%node)
  }
  else
  {
    $parser->YYData->{ERRMSG} = "Node parents are unbalanced";
    $parser->YYError;
    return undef;
  }

  # Just 'CALL' nodes except 'ROOT' can be parents.
  if ($node{'type'} and $node{'type'} eq 'CALL')
  {
    # 'SKIP' doesn't start a parent.
    if (!$node{'kind'} or ($node{'kind'} and $node{'kind'} ne 'SKIP'))
    {
      push(@{$parser->YYData->{PARENTS}}, \%node);
    }
  }

  # 'RETURN' node finishes a current parent.
  pop(@{$parser->YYData->{PARENTS}})
   if ($node{'type'} and $node{'type'} eq 'RETURN');

  return \%node;
}
	],
	[#Rule 5
		 'line', 0, undef
	],
	[#Rule 6
		 'line', 1,
sub
#line 109 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 7
		 'file', 0, undef
	],
	[#Rule 8
		 'file', 1,
sub
#line 112 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 9
		 'type', 0, undef
	],
	[#Rule 10
		 'type', 1,
sub
#line 115 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 11
		 'kind', 0, undef
	],
	[#Rule 12
		 'kind', 1,
sub
#line 118 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 13
		 'skip_reason', 0, undef
	],
	[#Rule 14
		 'skip_reason', 1,
sub
#line 121 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 15
		 'formal_arg_names_list', 0, undef
	],
	[#Rule 16
		 'formal_arg_names_list', 1,
sub
#line 124 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 17
		 'formal_arg_names', 2,
sub
#line 126 "shared/perl/modules/ETV/Parser.ym"
{ my @formal_arg_names = (@{$ARG[1]}, $ARG[2]); return \@formal_arg_names; }
	],
	[#Rule 18
		 'formal_arg_names', 1,
sub
#line 127 "shared/perl/modules/ETV/Parser.ym"
{ my @formal_arg_names = ($ARG[1]); return \@formal_arg_names; }
	],
	[#Rule 19
		 'arg_name', 1,
sub
#line 129 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	],
	[#Rule 20
		 'text', 0, undef
	],
	[#Rule 21
		 'text', 1,
sub
#line 132 "shared/perl/modules/ETV/Parser.ym"
{ return $ARG[1]; }
	]
],
                                  @_);
    bless($self,$class);
}

#line 134 "shared/perl/modules/ETV/Parser.ym"



1;

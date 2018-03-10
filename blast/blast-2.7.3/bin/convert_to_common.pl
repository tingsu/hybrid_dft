#!/usr/bin/perl -w
package ETV::Library;

use HTML::Entities;
use English;
use strict;
use FindBin;
# Add some local Perl packages.
use lib("$FindBin::RealBin/../etv/modules");

use ETV::Parser;
use LDV::Utils qw(vsay print_debug_warning print_debug_normal print_debug_info
  print_debug_debug print_debug_trace print_debug_all get_debug_level);

# Auxiliary error processing subroutine for the Yapp parser.
# args: Yapp parser.
# retn: nothing.
sub _Error($);
# Auxiliary lexer subroutine for the Yapp parser.
# args: Yapp parser.
# retn: token kind and value.
sub _Lexer($);

# A supported version of the error trace common format.
my $et_common_format = '0.1';

# Path to the converter.
my $engine = shift; 
my $converter = "$FindBin::RealBin/../etv/converters/$engine";

# File with original error trace.
my $orig_et_file = shift;

# Graphml witness file
my $graphml_file = shift;

# Should we put all available information to the trace
my $is_full = shift;

# File with processed error trace.
my $proc_et_file = shift;

# Read original error trace into the array.
open(ORIG_ET, '<', $orig_et_file)
	or die("Can't open file '$orig_et_file' for read: $ERRNO");
my @et_array = <ORIG_ET>;
close(ORIG_ET)
	or die("Can't close file handler for '$orig_et_file': $ERRNO\n");

# Here a converted error trace should be written to.
my @et_conv_array;

if (-f $converter)
{
	open(ETV_CONV, '<', $converter)
		or die("Can't open file '$converter' for read: $ERRNO");
	my $etv_conv = join("", <ETV_CONV>);
	close(ETV_CONV)
		or die("Can't close file handler for '$converter': $ERRNO\n");

	eval $etv_conv;
	die "Can't convert error trace by means of '$converter':  $@" if $@;
}
else
{
	die ("Converter '$converter' doesn't exist");
}

if($proc_et_file) {
	# Write processed trace to the file.
	open(PROC_ET, '>', $proc_et_file)
		or die("Can't open file '$proc_et_file' for write: $ERRNO");
	foreach my $line (@et_conv_array)
	{
		print PROC_ET "$line\n";
	}
	close(PROC_ET)
		or die("Can't close file handler for '$proc_et_file': $ERRNO\n");
}

print_debug_debug("A given error trace is in the common format");

my $et = {};
my $format;

print_debug_trace("Check that a given error trace is in the common format or"
    . " in one of supported formats or its format was provided via command-line"
    . " options");
$format = shift(@et_conv_array) unless ($format);
   if ($format =~ /^Error trace common format v(.+)$/
      and $1 eq $et_common_format)
    {
      print_debug_debug("A given error trace is in the common format"
        . " ('$et_common_format')");
    }
  else {
	die("Unsupported format $format");
  }
  

# Create and initialize a special common format parser.
my $parser = ETV::Parser->new();
$parser->YYData->{ET} = \@et_conv_array;
$parser->YYData->{LINE} = 1;
$parser->YYData->{FILE} = undef;

# Parse a error trace in the common format.
$et = $parser->YYParse(yylex => \&_Lexer, yyerror => \&_Error);



sub process_et_node($$);

#my $root_ref = $et;
my $root_id = 0;
my $id_ref = {'id' => $root_id};
my $etv = process_et_node($et, $id_ref);

# Write converted trace to graphml file.
open(GRAPHML, '>', $graphml_file)
	or die("Can't open file '$graphml_file' for write: $ERRNO");


my $header = << 'END_TXT';
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<graphml xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://graphml.graphdrawing.org/xmlns">
    <key attr.name="assumption" attr.type="string" for="edge" id="assumption"/>
    <key attr.name="sourcecode" attr.type="string" for="edge" id="sourcecode"/>
    <key attr.name="sourcecodeLanguage" attr.type="string" for="graph" id="sourcecodelang"/>
    <key attr.name="tokenSet" attr.type="string" for="edge" id="tokens"/>
    <key attr.name="originTokenSet" attr.type="string" for="edge" id="origintokens"/>
    <key attr.name="negativeCase" attr.type="string" for="edge" id="negated">
        <default>false</default>
    </key>
    <key attr.name="lineNumberInOrigin" attr.type="int" for="edge" id="startline"/>
    <key attr.name="lineNumberInOrigin" attr.type="int" for="edge" id="endline"/>
    <key attr.name="originFileName" attr.type="string" for="edge" id="originfile">
        <default>"&lt;command-line&gt;"</default>
    </key>
    <key attr.name="nodeType" attr.type="string" for="node" id="nodetype">
        <default>path</default>
    </key>
    <key attr.name="isFrontierNode" attr.type="boolean" for="node" id="frontier">
        <default>false</default>
    </key>
    <key attr.name="isViolationNode" attr.type="boolean" for="node" id="violation">
        <default>false</default>
    </key>
    <key attr.name="isEntryNode" attr.type="boolean" for="node" id="entry">
        <default>false</default>
    </key>
    <key attr.name="isSinkNode" attr.type="boolean" for="node" id="sink">
        <default>false</default>
    </key>
    <key attr.name="enterFunction" attr.type="string" for="edge" id="enterFunction"/>
    <key attr.name="returnFromFunction" attr.type="string" for="edge" id="returnFrom"/>
   <graph edgedefault="directed">
END_TXT

my $footer = << 'END_TXT';
    </graph>
</graphml>
END_TXT

print GRAPHML $header;

print GRAPHML "        <data key=\"sourcecodelang\">C</data>\n";
print GRAPHML "        <node id=\"$root_id\">\n";
print GRAPHML "              <data key=\"entry\">true</data>\n";

print GRAPHML (@{$etv});

print GRAPHML "              <data key=\"violation\">true</data>\n";
print GRAPHML "        </node>\n";
print GRAPHML $footer;

close(GRAPHML)
	or die("Can't close file handler for '$graphml_file': $ERRNO\n");


sub process_et_node($$)
{
  my $node_ref = shift;
  my $id_ref = shift;

  # Visualized representation for a given node.
  my @node_visualized = ();

  # This case can arise when a error trace is empty.
  return \@node_visualized if (!$node_ref);

  my %node = %{$node_ref};

  my $file = $node{'file'} // '';
  my $line = $node{'line'};
  my $type = $node{'type'} // '';
  my $kind = $node{'kind'} // '';
  my $formal_arg_names = $node{'formal_arg_names'} // [];
  my $skip_reason = $node{'skip_reason'} // '';
  my $text = $node{'text'} // '';
  my $text = encode_entities( $text );

  # skip INIT node (as far as it is not matched)
  if($kind eq 'INIT')
  {
    push(@node_visualized, "<!-- skip INIT node -->\n");
    return \@node_visualized;
  }

  if($line and ($type ne 'RETURN')) {
	# Id for a currently processed entity.
	my $parent_id = ${$id_ref}{'id'};
	my $id = ++${$id_ref}{'id'};
	if($text) {
		
	}
	push(@node_visualized,"        </node>\n");
	push(@node_visualized, "<!-- $type, $kind, ");
	push(@node_visualized, (@{$formal_arg_names}));
	push(@node_visualized, ",  $skip_reason -->\n");
	push(@node_visualized,"        <edge source=\"$parent_id\" target=\"$id\">\n");
	push(@node_visualized,"            <data key=\"startline\">$line</data>\n");
	if($is_full) {
		if($type eq 'CALL' and $kind ne 'SKIP') {
			push(@node_visualized,
			"            <data key=\"enterFunction\"></data>\n");
		}
		if($type eq 'ASSUME') {
			push(@node_visualized,
			"            <data key=\"assumption\">$text</data>\n");
		}
		push(@node_visualized,"            <data key=\"originfile\">\"$file\"</data>\n");
		push(@node_visualized,"            <data key=\"sourcecode\">$text</data>\n");
	}
	push(@node_visualized,"        </edge>\n");
	push(@node_visualized,"        <node id=\"$id\">\n");
  } else {
	if($is_full and ($type eq 'RETURN')) {
		my $parent_id = ${$id_ref}{'id'};
		my $id = ++${$id_ref}{'id'};
		push(@node_visualized,"        </node>\n");
		push(@node_visualized,"        <edge source=\"$parent_id\" target=\"$id\">\n");
		push(@node_visualized,"            <data key=\"startline\">$line</data>\n");
		push(@node_visualized,"            <data key=\"returnFrom\"></data>\n");
		push(@node_visualized,"            <data key=\"originfile\">\"$file\"</data>\n");
		push(@node_visualized,"            <data key=\"sourcecode\">$text</data>\n");
		push(@node_visualized,"        </edge>\n");
		push(@node_visualized,"        <node id=\"$id\">\n");
	}
	# skip  line
	push(@node_visualized, "<!-- skip: $type, $kind, ");
	push(@node_visualized, (@{$formal_arg_names}));
	push(@node_visualized, ",  $skip_reason, $text -->\n");
  }

  # Print all tree node children with enlarged indentation.
  if ($node{'children'})
  {
    #my $indent = $type eq 'ROOT' ? $visual{'indent'}: $visual{'indent'} + 1;
    #push(@node_visualized, "<!-- children begin -->\n");
    foreach my $child (@{$node{'children'}})
    {
      my $child_visualized_ref = process_et_node($child, $id_ref);
      push(@node_visualized, @{$child_visualized_ref});
    }
    #push(@node_visualized, "<!-- children end -->\n");
  }
  return \@node_visualized;
}


################################################################################
# Subroutines.
################################################################################

sub _Error($)
{
  my $parser = shift;

  if (my $errmsg = $parser->YYData->{ERRMSG})
  {
    print_debug_warning($errmsg);
    delete $parser->YYData->{ERRMSG};
    return;
  };

  print_debug_warning("Syntax error near token '"
    . $parser->YYCurtok . "' with current value '"
    . $parser->YYCurval . "' at line '"
    . $parser->YYData->{LINE} . "'");
}

sub _Lexer($)
{
  my $parser = shift;

  if (!$parser->YYData->{INPUT})
  {
    $parser->YYData->{INPUT} = read_next_line($parser);
    return ('', undef) if (!defined($parser->YYData->{INPUT}));
  }

  # Elimate all formatting characters from the beginning of string considered.
  $parser->YYData->{INPUT} =~ s/^[ \t]+//;

  # Skip comments elsewhere except in TEXT. Comments may be in php style.
  if ($parser->YYData->{INPUT} !~ /^\:/)
  {
    # Single-line comments, either '# ...' or '// ...'.
    $parser->YYData->{INPUT} =~ s/^(#|\/\/).*\n$/\n/;
    # Multi-line comments, '/* ... */'.
    if ($parser->YYData->{INPUT} =~ /^\/\*/)
    {
      while ($parser->YYData->{INPUT}
        and !($parser->YYData->{INPUT} =~ s/.*\*\///))
      {
        $parser->YYData->{INPUT} = read_next_line($parser);
      }
    }
  }

  # Read a next token.
  for ($parser->YYData->{INPUT})
  {
    s/^(\d+)// and return ('LINE', $1);
    s/^\"([^\"]+)\"// and return ('FILE', $1);
    s/^(BLOCK|DECLARATION|CALL|ASSUME|RETURN|NOP)// and return ('TYPE', $1);
    s/^(INIT|ENTRY|SKIP)// and return ('KIND', $1);
    s/^\(\"([^\"]+)\"\)// and return ('SKIP_REASON', $1);
    s/^\'([^\']+)\'// and return ('ARG_NAME', $1);
    s/^\:\s*(.*)\s*\n?$/\n/ and return ('TEXT', $1);
    s/^(.)//s and return ($1, $1);
  }
}

sub read_next_line($)
{
  my $parser = shift;

  $parser->YYData->{LINE}++;

  my $next_line = shift(@{$parser->YYData->{ET}});

  return undef unless (defined($next_line));
  return "$next_line\n";
}



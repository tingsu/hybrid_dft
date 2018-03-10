################################################################################
# Copyright (C) 2010-2012
# Institute for System Programming, Russian Academy of Sciences (ISPRAS).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
################################################################################
package ETV::Entity;

use English;
use strict;

use ETV::Annotation;


my $engine_blast = 'blast';

my %parent_entities = ($engine_blast => {'FunctionCall' => 1, 'FunctionCallInitialization' => 1});
my %parent_end_entities = ($engine_blast => {'Return' => 1, 'FunctionCallWithoutBody' => 1, 'FunctionStackOverflow' => 1});


sub new($$)
{
  my ($class, $data) = @ARG;

  my $init = ${$data}{'engine'};
  my $init_func = \&$init;

  my $self = $init_func->($data);
  bless $self, $class;

  return $self;
}

sub blast($)
{
  my $init = shift;
  # Blast has some special entities. Make them here.
  # The return block.
  if (${$init}{'kind'} eq 'Block')
  {
    if (${${$init}{'values'}}[0] =~ /^Return\((.*)\);$/)
    {
      ${$init}{'kind'} = 'Return';
      ${${$init}{'values'}}[0] = $1;
    }
  }
  # The initialization.
  elsif (${$init}{'kind'} eq 'FunctionCall')
  {
    if (${${$init}{'values'}}[0] =~ /^__BLAST_initialize_/)
    {
      ${$init}{'kind'} = 'FunctionCallInitialization';
    }
  }

  return $init;
}

sub ismay_have_children($)
{
  my $self = shift;

  return defined($parent_entities{$self->{'engine'}}{$self->{'kind'}});
}

sub isparent_end($)
{
  my $self = shift;

  # To prevent multiple parent ends that are made for multiple post annotations
  # specify whether entity was already ended and check it.
  unless ($self->{'entity was ended'})
    {
      if ($parent_end_entities{$self->{'engine'}}{$self->{'kind'}})
        {
          $self->{'entity was ended'} = 1;
          return 1;
        }
    }

  return 0;
}

sub set_parent($$)
{
  my ($self, $parent) = @ARG;

  # Store children to the parent.
  my @children = ();
  @children = @{$parent->{'children'}} if ($parent->{'children'});
  push(@children, $self);
  $parent->{'children'} = \@children;

  return $self;
}

sub set_post_annotations($@)
{
  my ($self, @post_annotations) = @ARG;

  $self->{'post annotations'} = \@post_annotations;

  # Change the entity kind if post annotation require this.
  foreach my $post_annotation (@post_annotations)
  {
    if (${$post_annotation}{'kind'} eq 'LDV')
    {
      if (${${$post_annotation}{'values'}}[0] and ${${$post_annotation}{'values'}}[0] =~ /undefined function call/)
      {
        $self->{'kind'} = 'FunctionCallWithoutBody';
      }

      if (${${$post_annotation}{'values'}}[0] and ${${$post_annotation}{'values'}}[0] =~ /skipping call to function due to stack overflow \((\d+)\)/)
      {
        $self->{'kind'} = 'FunctionStackOverflow';
        $self->{'fdepth'} = $1;
      }
    }
  }

  return $self;
}

sub set_pre_annotations($@)
{
  my ($self, @pre_annotations) = @ARG;

  $self->{'pre annotations'} = \@pre_annotations;

  return $self;
}

1;

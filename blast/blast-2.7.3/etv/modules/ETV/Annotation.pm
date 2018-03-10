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
package ETV::Annotation;

use English;
use strict;


my $engine_blast = 'blast';

my %post_annotations = ($engine_blast => {'LDV' => 1, 'Locals' => 1});
my %pre_annotations = ($engine_blast => {'Location' => 1});


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

  return $init;
}

sub ispost_annotation($)
{
  my $self = shift;

  return defined($post_annotations{$self->{'engine'}}{$self->{'kind'}});
}

sub ispre_annotation($)
{
  my $self = shift;

  return defined($pre_annotations{$self->{'engine'}}{$self->{'kind'}});
}

1;

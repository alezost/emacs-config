# Makefile

# Copyright (C) 2016 Alex Kost <alezost@gmail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# This file is used to byte-compile various *.el files of my Emacs
# config so that I can see and fix compilation warnings.

# Code:

EMACS = emacs

MY_INIT_DIR = $(CURDIR)/init
MY_UTILS_DIR = $(CURDIR)/utils
MY_ELPA_DIR = $(CURDIR)/packages
GUIX_DIR = $(HOME)/.guix-profiles/emacs/emacs/share/emacs/site-lisp
GUIX_ELPA_DIR = $(GUIX_DIR)/guix.d
EMACS_ELPA_DIR = $(HOME)/.emacs.d/elpa

L_dirs = $(shell find -L $(1) -mindepth 1 -maxdepth 1 -type d	\
                 -exec echo -L {} \;)

LOAD_PATH =					\
  -L $(MY_UTILS_DIR)				\
  -L $(GUIX_DIR)				\
  $(call L_dirs,$(MY_ELPA_DIR))			\
  $(call L_dirs,$(GUIX_ELPA_DIR))		\
  $(call L_dirs,$(EMACS_ELPA_DIR))

EMACS_BATCH = $(EMACS) -batch -Q $(LOAD_PATH)

UTILS_ELS = $(shell find -L $(MY_UTILS_DIR) -maxdepth 1 -name 'al-*el')
UTILS_ELCS = $(UTILS_ELS:.el=.elc)

# "init.el" and "keys.el" should be compiled first.
INIT_ELS =							\
  $(MY_INIT_DIR)/init.el					\
  $(MY_INIT_DIR)/keys.el					\
  $(shell find -L $(MY_INIT_DIR) -maxdepth 1 -name '*el'	\
          ! -name 'init.el' ! -name 'keys.el')
INIT_ELCS = $(INIT_ELS:.el=.elc)

all: $(UTILS_ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) -f batch-byte-compile $< ; \
	true  # to ignore possible compilation error and proceed with making

# This target is not very useful actually: compiling init files brings
# tons of garbage warnings because 'with-eval-after-load' wraps lambda
# in 'eval-after-load', so almost everything inside it is unknown (as it
# is used to set up a foreign package) and produces a warning.
init: $(INIT_ELS)
	@printf "Compiling init files\n"
	@$(EMACS_BATCH) -f batch-byte-compile $^ ;

clean-utils:
	@printf "Removing utils/*.elc...\n"
	$(RM) $(UTILS_ELCS)

clean-init:
	@printf "Removing init/*.elc...\n"
	$(RM) $(INIT_ELCS)

clean: clean-utils clean-init

.PHONY: all init clean clean-utils clean-init

# Makefile ends here

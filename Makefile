# Makefile

# Copyright (C) 2016-2026 Alex Kost <alezost@gmail.com>

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

# This file is not intended to be used to "install" my config in any
# way.  Its only purpose is to byte-compile various *.el files so that I
# can see and fix compilation warnings.

# Code:

EMACS = emacs

MY_INIT_DIR = $(CURDIR)/init
MY_UTILS_DIR = $(CURDIR)/utils
MY_ELPA_DIR = $(CURDIR)/packages
MY_GUIXEL_DIR = $(CURDIR)/packages/guix/elisp
MY_DATA_DIR = $(CURDIR)/data
EMACS_ELPA_DIR = $(MY_DATA_DIR)/elpa
GUIX_DIR = $(HOME)/.guix-profiles/emacs/emacs/share/emacs/site-lisp
GUIX_ELPA_DIR = $(GUIX_DIR)/guix.d
SLIME_DIR = $(shell find "$(HOME)/.quicklisp/dists/quicklisp/software" -mindepth 1 -maxdepth 1 -type d -name "slime*" | head -n1)
SLIME_CONTRIB_DIR = $(SLIME_DIR)/contrib

L_dirs = $(shell test -d $(1) &&				\
                 find -L $(1) -mindepth 1 -maxdepth 1 -type d	\
                 -exec echo -L {} \;)

LOAD_PATH =							\
  -L $(MY_ELPA_DIR)						\
  $(call L_dirs,$(MY_UTILS_DIR))				\
  $(call L_dirs,$(MY_ELPA_DIR))					\
  -L $(MY_GUIXEL_DIR)						\
  -L $(GUIX_DIR)						\
  $(call L_dirs,$(GUIX_ELPA_DIR))				\
  $(if $(SLIME_DIR), -L $(SLIME_CONTRIB_DIR) -L $(SLIME_DIR))	\
  $(call L_dirs,$(EMACS_ELPA_DIR))

EMACS_BATCH = $(EMACS) -batch -Q $(LOAD_PATH)

UTILS_ELS = $(shell find -L $(MY_UTILS_DIR) -mindepth 2 -name '*.el')
UTILS_ELCS = $(UTILS_ELS:.el=.elc)

PACKAGES_ELS = $(shell find -L $(MY_ELPA_DIR) -maxdepth 1 -name '*.el')
PACKAGES_ELCS = $(PACKAGES_ELS:.el=.elc)

INIT_ELS = $(shell find -L $(MY_INIT_DIR) -maxdepth 1 -name '*.el')
INIT_ELCS = $(INIT_ELS:.el=.elc)

packages+utils: packages utils

all: packages utils init

packages: $(PACKAGES_ELCS)

utils: $(UTILS_ELCS)

%.elc: %.el
	@printf "⏺ Compiling $< ...\n"
	-@$(EMACS_BATCH) --eval "(setq load-prefer-newer t)" \
	-f batch-byte-compile $< ;

# The main purpose of "init" target is to check for useful compilation
# warnings (like obsolete variables).  Also different macros are used
# heavily in init files, so compilation makes sense to avoid
# macro-expansion during loading.
init: $(INIT_ELCS)

clean-packages:
	@printf "Removing packages/*.elc...\n"
	$(RM) $(PACKAGES_ELCS)

clean-utils:
	@printf "Removing utils/*.elc...\n"
	$(RM) $(UTILS_ELCS)

clean-init:
	@printf "Removing init/*.elc...\n"
	$(RM) $(INIT_ELCS)

clean: clean-packages clean-utils

clean-all: clean-packages clean-utils clean-init

.PHONY: all init utils packages packages+utils
.PHONY: clean clean-all clean-init clean-utils clean-packages

# Makefile ends here

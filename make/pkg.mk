REPO_TAG := $(shell git describe --tags --always)
MAJOR_VERSION ?= $(shell echo $(REPO_TAG) | sed -e 's/\(.*\)-[^-]*-[^-]*/\1/')
PKG_BUILD        = 1
ERLANG_BIN       = $(shell dirname $(shell which erl))

archive_git = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

CLONEDIR ?= $(APP)-clone
MANIFEST_FILE ?= dependency_manifest.git
get_dist_deps = mkdir distdir && \
                git clone . distdir/$(CLONEDIR) && \
                cd distdir/$(CLONEDIR) && \
                git checkout $(REPO_TAG) && \
                $(1) get-deps && \
                echo "- Dependencies and their tags at build time of $(REPO) at $(REPO_TAG)" > $(MANIFEST_FILE) && \
                for dep in deps/*; do \
                    cd $${dep} && \
                    printf "$${dep} version `git describe --always --long --tags 2>/dev/null || git rev-parse HEAD`\n" >> ../../$(MANIFEST_FILE) && \
                    cd ../..; done && \
                LC_ALL=POSIX && export LC_ALL && sort $(MANIFEST_FILE) > $(MANIFEST_FILE).tmp && mv $(MANIFEST_FILE).tmp $(MANIFEST_FILE);

NAME_HASH = $(shell git hash-object distdir/$(CLONEDIR)/$(MANIFEST_FILE) 2>/dev/null | cut -c 1-8)
CURRENT_BRANCH := $(shell git branch --no-color 2> /dev/null | grep \* | cut -d " " -f 2)
ifeq (master, $(CURRENT_BRANCH))
PKG_VERSION = $(MAJOR_VERSION)
PKG_ID = $(APP)-$(MAJOR_VERSION)
else
ifeq ($(REPO_TAG), $(MAJOR_VERSION))
PKG_VERSION = $(REPO_TAG)
PKG_ID = $(APP)-$(REPO_TAG)
else
META = $(subst -,.,$(shell echo $(REPO_TAG) | awk 'BEGIN{FS="-"}{print $$(NF-1)}'))
PKG_VERSION = $(MAJOR_VERSION)+build.$(META).$(NAME_HASH)
PKG_ID = $(APP)-$(PKG_VERSION)
DEV=true
endif
endif

build_clean_dir = cd distdir/$(CLONEDIR) && \
                  $(call archive_git,$(PKG_ID),..) && \
                  cp $(MANIFEST_FILE) $(REBAR) ../$(PKG_ID)/ && \
                  for dep in deps/*; do \
                      cd $${dep} && \
                      $(call archive_git,$${dep},../../../$(PKG_ID)) && \
                      mkdir -p ../../../$(PKG_ID)/$${dep}/priv && \
                      printf "`git describe --always --long --tags 2>/dev/null || git rev-parse HEAD`" > ../../../$(PKG_ID)/$${dep}/priv/vsn.git && \
                      cd ../..; done

distdir/$(CLONEDIR)/$(MANIFEST_FILE): $(REBAR)
	$(call get_dist_deps,$(REBAR))

distdir/$(PKG_ID): distdir/$(CLONEDIR)/$(MANIFEST_FILE)
	$(call build_clean_dir)

distdir/$(PKG_ID).tar.gz: distdir/$(PKG_ID)
	tar -C distdir -czf distdir/$(PKG_ID).tar.gz $(PKG_ID)

dist: distdir/$(PKG_ID).tar.gz
	cp distdir/$(PKG_ID).tar.gz .

ballclean:
	rm -rf $(PKG_ID).tar.gz distdir

pkgclean: ballclean
	rm -rf package

package: distdir/$(PKG_ID).tar.gz
	ln -sf distdir package
	$(MAKE) -C package -f $(PKG_ID)/deps/node_package/Makefile

.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR RELEASE

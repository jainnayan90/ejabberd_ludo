REBAR = /opt/homebrew/lib/erlang/bin/escript rebar
#REBAR = /usr/local/Cellar/erlang\@22/22.3.4.12/lib/erlang/bin/escript rebar
INSTALL = /usr/bin/install -c
SED = /usr/bin/sed
ERL = /usr/local/bin/erl

prefix = /Users/nayanjain/ejabberd_make/ejabberd_ludo
exec_prefix = ${prefix}

DESTDIR =

# /etc/ejabberd/
ETCDIR = $(DESTDIR)${prefix}/etc/ejabberd

# /bin/
BINDIR = $(DESTDIR)${exec_prefix}/bin

# /sbin/
SBINDIR = $(DESTDIR)${exec_prefix}/sbin

# /lib/
LIBDIR =  $(DESTDIR)${exec_prefix}/lib

# /lib/ejabberd/
EJABBERDDIR = $(DESTDIR)${exec_prefix}/lib/ejabberd

# /share/doc/ejabberd
PACKAGE_TARNAME = ejabberd
datarootdir = ${prefix}/share
DOCDIR = $(DESTDIR)${datarootdir}/doc/${PACKAGE_TARNAME}

# /usr/lib/ejabberd/ebin/
BEAMDIR = $(EJABBERDDIR)/ebin

# /usr/lib/ejabberd/include/
INCLUDEDIR = $(EJABBERDDIR)/include

# /usr/lib/ejabberd/priv/
PRIVDIR = $(EJABBERDDIR)/priv

# /usr/lib/ejabberd/priv/bin
PBINDIR = $(PRIVDIR)/bin

# /usr/lib/ejabberd/priv/lib
SODIR = $(PRIVDIR)/lib

# /usr/lib/ejabberd/priv/msgs
MSGSDIR = $(PRIVDIR)/msgs

# /usr/lib/ejabberd/priv/css
CSSDIR = $(PRIVDIR)/css

# /usr/lib/ejabberd/priv/img
IMGDIR = $(PRIVDIR)/img

# /usr/lib/ejabberd/priv/js
JSDIR = $(PRIVDIR)/js

# /usr/lib/ejabberd/priv/sql
SQLDIR = $(PRIVDIR)/sql

# /usr/lib/ejabberd/priv/lua
LUADIR = $(PRIVDIR)/lua

# /var/lib/ejabberd/
SPOOLDIR = $(DESTDIR)${prefix}/var/lib/ejabberd

# /var/lock/ejabberdctl
CTLLOCKDIR = $(DESTDIR)${prefix}/var/lock/ejabberdctl

# /var/lib/ejabberd/.erlang.cookie
COOKIEFILE = $(SPOOLDIR)/.erlang.cookie

# /var/log/ejabberd/
LOGDIR = $(DESTDIR)${prefix}/var/log/ejabberd

INSTALLUSER=
# if no user was enabled, don't set privileges or ownership
ifeq ($(INSTALLUSER),)
  O_USER=
  G_USER=
  CHOWN_COMMAND=echo
  CHOWN_OUTPUT=/dev/null
  INIT_USER=root
else
  O_USER=-o $(INSTALLUSER)
  G_USER=-g $(INSTALLUSER)
  CHOWN_COMMAND=chown
  CHOWN_OUTPUT=&1
  INIT_USER=$(INSTALLUSER)
endif
# if no group was enabled, don't set privileges or ownership
INSTALLGROUP=
ifneq ($(INSTALLGROUP),)
  G_USER=-g $(INSTALLGROUP)
endif

all: deps src

deps: deps/.got

deps/.got:
	rm -rf deps/.got
	rm -rf deps/.built
	mkdir -p deps
	$(REBAR) get-deps && :> deps/.got

deps/.built: deps/.got
	$(REBAR) configure-deps
	$(REBAR) compile && :> deps/.built

src: deps/.built
	$(REBAR) skip_deps=true compile

update:
	rm -rf deps/.got
	rm -rf deps/.built
	$(REBAR) update-deps && :> deps/.got

xref: all
	$(REBAR) skip_deps=true xref


translations:
	tools/prepare-tr.sh

edoc:
	$(ERL) -noinput +B -eval \
        'case edoc:application(ejabberd, ".", []) of ok -> halt(0); error -> halt(1) end.'

JOIN_PATHS=$(if $(wordlist 2,1000,$(1)),$(firstword $(1))/$(call JOIN_PATHS,$(wordlist 2,1000,$(1))),$(1))

VERSIONED_DEP=$(if $(DEP_$(1)_VERSION),$(DEP_$(1)_VERSION),$(1))

ELIXIR_TO_DEST=$(LIBDIR) $(call VERSIONED_DEP,$(word 2,$(1))) $(wordlist 5,1000,$(1))
DEPS_TO_DEST=$(LIBDIR) $(call VERSIONED_DEP,$(word 2,$(1))) $(wordlist 3,1000,$(1))
MAIN_TO_DEST=$(LIBDIR) $(call VERSIONED_DEP,ejabberd) $(1)
TO_DEST_SINGLE=$(if $(subst XdepsX,,X$(word 1,$(1))X),$(call MAIN_TO_DEST,$(1)),$(if $(subst XlibX,,X$(word 3,$(1))X),$(call DEPS_TO_DEST,$(1)),$(call ELIXIR_TO_DEST,$(1))))
TO_DEST=$(foreach path,$(1),$(call JOIN_PATHS,$(call TO_DEST_SINGLE,$(subst /, ,$(path)))))

FILTER_DIRS=$(foreach path,$(1),$(if $(wildcard $(path)/*),,$(path)))
FILES_WILDCARD=$(call FILTER_DIRS,$(foreach w,$(1),$(wildcard $(w))))

ifeq ($(MAKECMDGOALS),copy-files-sub)

DEPS:=$(sort $(shell $(REBAR) -q list-deps|$(SED) -ne '/ TAG / s/ .*// p; / REV / s/ .*// p; / BRANCH / s/ .*// p'))

DEPS_FILES=$(call FILES_WILDCARD,$(foreach DEP,$(DEPS),deps/$(DEP)/ebin/*.beam deps/$(DEP)/ebin/*.app deps/$(DEP)/priv/* deps/$(DEP)/priv/lib/* deps/$(DEP)/priv/bin/* deps/$(DEP)/include/*.hrl deps/$(DEP)/COPY* deps/$(DEP)/LICENSE* deps/$(DEP)/lib/*/ebin/*.beam deps/$(DEP)/lib/*/ebin/*.app))

BINARIES=deps/epam/priv/bin/epam deps/eimp/priv/bin/eimp deps/fs/priv/mac_listener

DEPS_FILES_FILTERED=$(filter-out $(BINARIES) deps/elixir/ebin/elixir.app,$(DEPS_FILES))
DEPS_DIRS=$(sort deps/ $(foreach DEP,$(DEPS),deps/$(DEP)/) $(dir $(DEPS_FILES)))

MAIN_FILES=$(filter-out %/configure.beam,$(call FILES_WILDCARD,ebin/*.beam ebin/*.app priv/msgs/*.msg priv/css/*.css priv/img/*.png priv/js/*.js priv/lib/* include/*.hrl COPYING))
MAIN_DIRS=$(sort $(dir $(MAIN_FILES)) priv/bin priv/sql priv/lua)

define DEP_VERSION_template
DEP_$(1)_VERSION:=$(shell $(SED) -e '/vsn/!d;s/.*, *"/$(1)-/;s/".*//' $(2) 2>/dev/null)
endef

DELETE_TARGET_SO=$(if $(subst X.soX,,X$(suffix $(1))X),,rm -f $(call TO_DEST,$(1));)

$(foreach DEP,$(DEPS),$(eval $(call DEP_VERSION_template,$(DEP),deps/$(DEP)/ebin/$(DEP).app)))
$(eval $(call DEP_VERSION_template,ejabberd,ebin/ejabberd.app))

define COPY_template
$(call TO_DEST,$(1)): $(1) $(call TO_DEST,$(dir $(1))) ; $(call DELETE_TARGET_SO, $(1)) $$(INSTALL) -m 644 $(1) $(call TO_DEST,$(1))
endef

define COPY_BINARY_template
$(call TO_DEST,$(1)): $(1) $(call TO_DEST,$(dir $(1))) ; rm -f $(call TO_DEST,$(1)); $$(INSTALL) -m 755 $$(O_USER) $(1) $(call TO_DEST,$(1))
endef

$(foreach file,$(DEPS_FILES_FILTERED) $(MAIN_FILES),$(eval $(call COPY_template,$(file))))

$(foreach file,$(BINARIES),$(eval $(call COPY_BINARY_template,$(file))))

$(sort $(call TO_DEST,$(MAIN_DIRS) $(DEPS_DIRS))):
	$(INSTALL) -d $@

$(call TO_DEST,priv/sql/lite.sql): sql/lite.sql $(call TO_DEST,priv/sql)
	$(INSTALL) -m 644 $< $@

$(call TO_DEST,priv/bin/captcha.sh): tools/captcha.sh $(call TO_DEST,priv/bin)
	$(INSTALL) -m 755 $(O_USER) $< $@

$(call TO_DEST,priv/lua/redis_sm.lua): priv/lua/redis_sm.lua $(call TO_DEST,priv/lua)
	$(INSTALL) -m 644 $< $@

copy-files-sub2: $(call TO_DEST,$(DEPS_FILES) $(MAIN_FILES) priv/bin/captcha.sh priv/sql/lite.sql priv/lua/redis_sm.lua)

.PHONY: $(call TO_DEST,$(DEPS_FILES) $(MAIN_DIRS) $(DEPS_DIRS))

endif

copy-files:
	$(MAKE) copy-files-sub

copy-files-sub: copy-files-sub2

install: all copy-files
	#
	# Configuration files
	$(INSTALL) -d -m 750 $(G_USER) $(ETCDIR)
	[ -f $(ETCDIR)/ejabberd.yml ] \
		&& $(INSTALL) -b -m 640 $(G_USER) ejabberd.yml.example $(ETCDIR)/ejabberd.yml-new \
		|| $(INSTALL) -b -m 640 $(G_USER) ejabberd.yml.example $(ETCDIR)/ejabberd.yml
	$(SED) -e "s*{{rootdir}}*/Users/nayanjain/ejabberd_make/ejabberd_ludo*g" \
		-e "s*{{installuser}}**g" \
		-e "s*{{bindir}}*${exec_prefix}/bin*g" \
		-e "s*{{libdir}}*${exec_prefix}/lib*g" \
		-e "s*{{sysconfdir}}*${prefix}/etc*g" \
		-e "s*{{localstatedir}}*${prefix}/var*g" \
		-e "s*{{docdir}}*${datarootdir}/doc/${PACKAGE_TARNAME}*g" \
		-e "s*{{erl}}*/usr/local/bin/erl*g" \
		-e "s*{{epmd}}*/usr/local/bin/epmd*g" ejabberdctl.template \
		> ejabberdctl.example
	[ -f $(ETCDIR)/ejabberdctl.cfg ] \
		&& $(INSTALL) -b -m 640 $(G_USER) ejabberdctl.cfg.example $(ETCDIR)/ejabberdctl.cfg-new \
		|| $(INSTALL) -b -m 640 $(G_USER) ejabberdctl.cfg.example $(ETCDIR)/ejabberdctl.cfg
	$(INSTALL) -b -m 644 $(G_USER) inetrc $(ETCDIR)/inetrc
	#
	# Administration script
	[ -d $(SBINDIR) ] || $(INSTALL) -d -m 755 $(SBINDIR)
	$(INSTALL) -m 550 $(G_USER) ejabberdctl.example $(SBINDIR)/ejabberdctl
	# Elixir binaries
	[ -d $(BINDIR) ] || $(INSTALL) -d -m 755 $(BINDIR)
	[ -f deps/elixir/bin/iex ] && $(INSTALL) -m 550 $(G_USER) deps/elixir/bin/iex $(BINDIR)/iex || true
	[ -f deps/elixir/bin/elixir ] && $(INSTALL) -m 550 $(G_USER) deps/elixir/bin/elixir $(BINDIR)/elixir || true
	[ -f deps/elixir/bin/mix ] && $(INSTALL) -m 550 $(G_USER) deps/elixir/bin/mix $(BINDIR)/mix || true
	#
	# Init script
	$(SED) -e "s*@ctlscriptpath@*$(SBINDIR)*g" \
		-e "s*@installuser@*$(INIT_USER)*g" ejabberd.init.template \
		> ejabberd.init
	chmod 755 ejabberd.init
	#
	# Service script
	$(SED) -e "s*@ctlscriptpath@*$(SBINDIR)*g" ejabberd.service.template \
		> ejabberd.service
	chmod 644 ejabberd.service
	#
	# Spool directory
	$(INSTALL) -d -m 750 $(O_USER) $(SPOOLDIR)
	$(CHOWN_COMMAND) -R  $(SPOOLDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(SPOOLDIR)
	[ ! -f $(COOKIEFILE) ] || { $(CHOWN_COMMAND)  $(COOKIEFILE) >$(CHOWN_OUTPUT) ; chmod 400 $(COOKIEFILE) ; }
	#
	# ejabberdctl lock directory
	$(INSTALL) -d -m 750 $(O_USER) $(CTLLOCKDIR)
	$(CHOWN_COMMAND) -R  $(CTLLOCKDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(CTLLOCKDIR)
	#
	# Log directory
	$(INSTALL) -d -m 750 $(O_USER) $(LOGDIR)
	$(CHOWN_COMMAND) -R  $(LOGDIR) >$(CHOWN_OUTPUT)
	chmod -R 750 $(LOGDIR)
	#
	# Documentation
	$(INSTALL) -d $(DOCDIR)
	[ -f doc/guide.html ] \
		&& $(INSTALL) -m 644 doc/guide.html $(DOCDIR) \
		|| echo "Documentation not included in sources"
	$(INSTALL) -m 644 COPYING $(DOCDIR)

uninstall: uninstall-binary

uninstall-binary:
	rm -f  $(SBINDIR)/ejabberdctl
	rm -f  $(BINDIR)/iex
	rm -f  $(BINDIR)/elixir
	rm -f  $(BINDIR)/mix
	rm -fr $(DOCDIR)
	rm -f  $(BEAMDIR)/*.beam
	rm -f  $(BEAMDIR)/*.app
	rm -fr $(BEAMDIR)
	rm -f  $(INCLUDEDIR)/*.hrl
	rm -fr $(INCLUDEDIR)
	rm -fr $(PBINDIR)
	rm -f  $(SODIR)/*.so
	rm -fr $(SODIR)
	rm -f  $(MSGSDIR)/*.msg
	rm -fr $(MSGSDIR)
	rm -f  $(CSSDIR)/*.css
	rm -fr $(CSSDIR)
	rm -f  $(IMGDIR)/*.png
	rm -fr $(IMGDIR)
	rm -f  $(JSDIR)/*.js
	rm -fr $(JSDIR)
	rm -f  $(SQLDIR)/*.sql
	rm -fr $(SQLDIR)
	rm -fr $(LUADIR)/*.lua
	rm -fr $(LUADIR)
	rm -fr $(PRIVDIR)
	rm -fr $(EJABBERDDIR)

uninstall-all: uninstall-binary
	rm -rf $(ETCDIR)
	rm -rf $(EJABBERDDIR)
	rm -rf $(SPOOLDIR)
	rm -rf $(CTLLOCKDIR)
	rm -rf $(LOGDIR)

clean:
	rm -rf deps/.got
	rm -rf deps/.built
	rm -rf test/*.beam
	$(REBAR) clean

clean-rel:
	rm -rf rel/ejabberd

distclean: clean clean-rel
	rm -f config.status
	rm -f config.log
	rm -rf autom4te.cache
	rm -rf deps
	rm -rf ebin
	rm -f Makefile
	rm -f vars.config
	rm -f src/ejabberd.app.src
	[ ! -f ../ChangeLog ] || rm -f ../ChangeLog

rel: all
	$(REBAR) generate

TAGS:
	etags *.erl

Makefile: Makefile.in

deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib sasl crypto \
	public_key ssl mnesia inets odbc tools compiler erts \
	runtime_tools asn1 observer xmerl et gs wx syntax_tools; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/ejabberd.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/ejabberd.plt \
	-o dialyzer/ejabberd.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

ejabberd_plt: dialyzer/ejabberd.plt
	@dialyzer --plt dialyzer/ejabberd.plt --check_plt -o dialyzer/ejabberd.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt ejabberd_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

test:
	@echo "************************** NOTICE ***************************************"
	@cat test/README
	@echo "*************************************************************************"
	@cd priv && ln -sf ../sql
	$(REBAR) skip_deps=true ct

quicktest:
	$(REBAR) skip_deps=true ct suites=elixir

.PHONY: src edoc dialyzer Makefile TAGS clean clean-rel distclean rel \
	install uninstall uninstall-binary uninstall-all translations deps test \
	quicktest erlang_plt deps_plt ejabberd_plt

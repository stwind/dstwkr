## copied from https://github.com/opscode/concrete
BASIC_PLT := ~/.dialyzer_plt
DIALYZER := dialyzer
DIALYZER_APPS = asn1 \
				compiler \
				crypto \
				edoc \
				erts \
				eunit \
				gs \
				hipe \
				inets \
				kernel \
				mnesia \
				observer \
				public_key \
				runtime_tools \
				ssl \
				stdlib \
				syntax_tools \
				tools

ALL_DEPS = $(notdir $(wildcard deps/*))
DEPS_LIST = $(filter-out $(DIALYZER_SKIP_DEPS) rebar, $(ALL_DEPS))
DIALYZER_DEPS = $(foreach dep,$(DEPS_LIST),deps/$(dep)/ebin)
DEPS_PLT = deps.plt
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: $(BASIC_PLT)
	@dialyzer $(DIALYZER_OPTS) -r ebin
else
dialyzer: $(BASIC_PLT) $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

$(BASIC_PLT):
	@echo "building $(BASIC_PLT) ..."
	@$(DIALYZER) --build_plt --output_plt $(BASIC_PLT) --apps $(DIALYZER_APPS)


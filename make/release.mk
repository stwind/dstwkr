APP ?= demo_app

rel: app rel/$(APP) 

rel/$(APP):
	@cd rel && $(REBAR) generate $(OVERLAY_VARS) skip_deps=true

relclean:
	@rm -rf rel/$(APP)

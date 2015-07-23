$(REBAR):
	@mkdir -p $(DEPS)
	@[ ! -d $(DEPS)/rebar ] && git clone $(REBAR_REPO) $(DEPS)/rebar; $(MAKE) -C $(DEPS)/rebar
	@cp $(DEPS)/rebar/rebar .
 
rebar: $(REBAR)

REBAR=./rebar
REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar

compile:
	$(REBAR) compile

test: eunit proper

eunit:
	$(REBAR) eunit

proper:
	erl -pa ebin deps/proper/ebin -eval "proper:check_specs(lfu), halt()."

update-rebar:
	rm -f $(REBAR)
	wget -q -O $(REBAR) $(REBAR_URL)
	chmod u+x $(REBAR)

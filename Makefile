all: ebin/
	(cd src;$(MAKE) all)

edoc:
	(cd src;$(MAKE) edoc)

test:
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)

clean_plt:
	(cd src;$(MAKE) clean_plt)

run: clean all
	(cd ebin;erl -s pwnerer go)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin

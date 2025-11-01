build:
	@rebar3 compile

demo:
	@rebar3 demo

hex-publish:
	@echo "\nPublishing to hex.pm ...\n"
	rm -rf doc
	mkdir doc
	cp priv/html/docs-redirect.html doc/index.html
	rebar3 hex publish
	rm -rf doc

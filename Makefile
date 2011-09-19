include vsn.mk

.PHONY: all docs clean test release

all:
	(cd src;$(MAKE))

docs:
	(cd src;$(MAKE) docs)

clean:
	(cd src;$(MAKE) clean)

test: all
	(cd test;$(MAKE))

release: all
	rm -fr releases/xmlrpc-$(VSN)
	mkdir releases/xmlrpc-$(VSN)
	install -m 644 LICENSE README releases/xmlrpc-$(VSN)
	mkdir releases/xmlrpc-$(VSN)/doc
	install -m 644 doc/xmlrpc.3 releases/xmlrpc-$(VSN)/doc
	install -m 644 doc/xmlrpc.txt releases/xmlrpc-$(VSN)/doc
	install -m 644 doc/xmlrpc.ps releases/xmlrpc-$(VSN)/doc
	install -m 644 doc/xmlrpc.pdf releases/xmlrpc-$(VSN)/doc
	mkdir releases/xmlrpc-$(VSN)/ebin
	mkdir releases/xmlrpc-$(VSN)/examples
	install -m 644 examples/*.erl examples/*.java examples/*.txt examples/README examples/Makefile releases/xmlrpc-$(VSN)/examples
	mkdir releases/xmlrpc-$(VSN)/src
	install -m 644 src/*.erl src/*.hrl releases/xmlrpc-$(VSN)/src
	install -m 644 src/Makefile releases/xmlrpc-$(VSN)/src
	(cd releases;tar zcvf xmlrpc-$(VSN).tgz xmlrpc-$(VSN))

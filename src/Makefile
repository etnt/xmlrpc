# Do not change anything below this line.
ifeq ($(DEBUG),true)
DEBUG_FLAGS = -DDEBUG
else
DEBUG_FLAGS =
endif

ERLC=erlc
ERLC_FLAGS=-W $(DEBUG_FLAGS) -o ../ebin
MODULES=xmlrpc xmlrpc_decode xmlrpc_encode xmlrpc_http xmlrpc_util xmlrpc_tcp_serv
EBIN_FILES=$(MODULES:%=../ebin/%.beam)
ERL_FILES=$(MODULES:%=%.erl)
INCLUDES=

all: $(EBIN_FILES)

../ebin/%.beam: %.erl log.hrl
	$(ERLC) $(ERLC_FLAGS) $<

docs: ../doc/index.html

../doc/index.html: ../doc/overview.edoc $(ERL_FILES)  $(INCLUDES)
	erl -noshell -run edoc_run application "xmlrpc" '".."' '[]'

clean:
	rm -f $(EBIN_FILES) ../doc/*.html ../doc/edoc-info \
	  ../doc/stylesheet.css ../doc/erlang.png

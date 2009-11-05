#Compiles the code into a ebin dir. relative to the source dir. 
EBIN = ../ebin
DOC = ../doc
ERL = erl
ERLC = erlc
GEN = beam
#Where include files are stored ".hrl"
EFLAGS = -I../include
SOURCE = xmlrpc.erl xmlrpc_decode.erl xmlrpc_encode.erl xmlrpc_http.erl xmlrpc_util.erl tcp_serv.erl
TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	$(ERLC) -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

all: dirs $(TARGETS)

dirs:
	mkdir -p $(EBIN)
	mkdir -p $(DOC)

tags:
	../scripts/createtags.sh

clean:
	\rm -f $(CODE) 

doc: 
	rm -f ../doc/*.html
	rm -f ../doc/*.css
	rm -f ../doc/*.png
	rm -f ../doc/edoc-info
	../scripts/gendoc.sh $(SOURCE)
	mv *.html $(DOC)
	mv *.css $(DOC)
	mv *.png $(DOC)
	mv edoc-info $(DOC)
	open ../doc/index.html

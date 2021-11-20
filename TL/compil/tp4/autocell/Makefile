
# OCAML configuration
OCAML_PREFIX=
OCAMLC=$(OCAML_PREFIX)ocamlc
OCAMLLEX=$(OCAML_PREFIX)ocamllex
OCAMLYACC=$(OCAML_PREFIX)ocamlyacc


# configuration
#%CFLAGS=-I tiny_httpd/src -I coq -g
CFLAGS=-I tiny_httpd/src -g
LDFLAGS=tiny_httpd/tiny_httpd.cma
OCAMLYACC_FLAGS=--strict -v
OCAMLLEX_FLAGS=-ml


# definitions
#%SUBDIRS=tiny_httpd coq
SUBDIRS=tiny_httpd
APPS=\
	autocc \
	autocell \
	autoas \
	autoexec
#%astclean
CLEAN=

# rules
all: all-rec $(APPS)

# astclean
#%ASTCLEAN_SOURCES=\
#%	common.ml \
#%	ast.ml \
#%	coq/listSet.ml \
#%	coq/useless.ml \
#%	coq/wrtonce.ml \
#%	coq/transfo.ml \
#%	symbols.ml \
#%	lexer.ml \
#%	parser.ml \
#%	astpp.ml \
#%	astclean.ml

#%ASTCLEAN_OBJECTS=$(ASTCLEAN_SOURCES:.ml=.cmo)
#%CLEAN+= \
#%	$(ASTCLEAN_OBJECTS) \
#%	$(ASTCLEAN_SOURCES:.ml=.cmi) \
#%	astclean lexer.ml parser.ml parser.output

#% astclean: $(ASTCLEAN_OBJECTS)
#%	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^

#%coq/listSet.cmo: coq/listSet.cmi
#%coq/wrtonce.cmo: coq/wrtonce.cmi
#%coq/useless.cmo: coq/useless.cmi
#%coq/transfo.cmo: coq/transfo.cmi
#%coq/transfo.cmo: ast.cmo coq/useless.cmo coq/wrtonce.cmo coq/listSet.cmo
#%astpp.cmo:symbols.cmo parser.cmo
#%astclean.cmo: lexer.cmo parser.cmo common.cmo symbols.cmo coq/transfo.cmo

# autocc
AUTOCC_SOURCES=\
	common.ml \
	symbols.ml \
	ast.ml \
	quad.ml \
	vm.ml \
	cell.ml \
	comp.ml \
	lexer.ml \
	symbols.ml \
	parser.ml \
	autocc.ml
AUTOCC_OBJECTS=$(AUTOCC_SOURCES:.ml=.cmo)
CLEAN+= \
	$(AUTOCC_OBJECTS) \
	$(AUTOCC_SOURCES:.ml=.cmi) \
	autocc lexer.ml parser.ml parser.output

autocc: $(AUTOCC_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^
parser.cmo: symbols.cmo parser.cmi common.cmo ast.cmo comp.cmo
parser.cmi: parser.mly common.cmi
lexer.cmi: lexer.mll parser.cmi common.cmo
lexer.cmo: symbols.cmo lexer.cmi parser.cmi common.cmo
autocc.cmo: lexer.cmo parser.cmo comp.cmo common.cmo
ast.cmo: common.cmo
comp.cmo: symbols.cmo ast.cmo quad.cmo cell.ml common.cmo vm.cmo


# autocell
AUTOCELL_SOURCES=\
	common.ml \
	quad.ml \
	vm.ml \
	cell.ml \
	autocell.ml
AUTOCELL_OBJECTS=$(AUTOCELL_SOURCES:.ml=.cmo)
CLEAN+=$(AUTOCELL_OBJECTS) $(AUTOCELL_SOURCES:.ml=.cmi) autocell

autocell: $(AUTOCELL_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^
autocell.cmo: common.cmo quad.cmo cell.cmo vm.cmo


# autoas
AUTOAS_SOURCES=\
	common.cmo \
	quad.ml \
	aslexer.ml \
	asparser.ml \
	autoas.ml
AUTOAS_OBJECTS=$(AUTOAS_SOURCES:.ml=.cmo)
CLEAN+= \
	$(AUTOAS_OBJECTS) \
	$(AUTOAS_SOURCES:.ml=.cmi) \
	autoas asparser.ml aslexer.ml asparser.output

autoas: $(AUTOAS_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^
autoas.cmo: asparser.cmi asparser.cmo aslexer.cmo common.cmo
aslexer.cmo: asparser.cmi common.cmo
asparser.cmo: common.cmo quad.cmo
asparser.cmo: quad.cmo
asparser.cmi: quad.cmo


# autoexec
AUTOEXEC_SOURCES= \
	common.ml \
	quad.ml \
	vm.ml \
	cell.ml \
	autoexec.ml
AUTOEXEC_OBJECTS=$(AUTOEXEC_SOURCES:.ml=.cmo)
CLEAN+=$(AUTOCEXEC_OBJECTS) $(AUTOEXEC_SOURCES:.ml=.cmi) autoexec

autoexec: $(AUTOEXEC_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^
autoexec.cmo: cell.cmo vm.cmo quad.cmo common.cmo


# common dependencies
cell.cmo: vm.cmo common.cmo
vm.cmo: quad.cmo common.cmo


# generic rules
all-rec:
	@for sub in $(SUBDIRS); do \
		cd $$sub; $(MAKE) all || exit 1; cd ..; \
	done

%.cmo: %.ml
	$(OCAMLC) $(CFLAGS) -o $@ -c $<

%.cmi: %.mli
	$(OCAMLC) $(CFLAGS) -o $@ -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $(OCAMLYACC_FLAGS) $<

%.ml: %.mll
	$(OCAMLLEX) $(OCAMLLEX_FLAGS) $<


clean:
	@for sub in $(SUBDIRS); do \
		cd $$sub; $(MAKE) clean; cd ..; \
	done
	-rm -rf $(CLEAN) $(tiny_httpd).cma autos/*.s autos/*.exe


# generation and archiving
DIRNAME = autocell
REAL_AUTOCC_SOURCES = \
	$(subst lexer.ml, lexer.mll, \
		$(subst parser.ml, parser.mly, $(AUTOCC_SOURCES)))
REAL_AUTOAS_SOURCES = \
	$(subst aslexer.ml, aslexer.mll, \
		$(subst asparser.ml, asparser.mly, $(AUTOAS_SOURCES)))
ALL_SOURCES = $(sort \
	$(REAL_AUTOCC_SOURCES) \
	$(REAL_AUTOAS_SOURCES) \
	$(AUTOEXEC_SOURCES) \
	$(AUTOCELL_SOURCES)) \
	Makefile \
	maps \
	pages
TO_FILTER = \
	code/ex1.s \
	code/ex2.s \
	code/ex3.s \
	code/ex4.s \
	parser.mly \
	lexer.mll \
	comp.ml

generate:
	-rm -rf "$(DIRNAME)" fi
	mkdir "$(DIRNAME)"
	rsync -r . "$(DIRNAME)" --exclude-from rsync.exclude
	mkdir "$(DIRNAME)/code"
	for f in $(TO_FILTER); do ./filter.py < $$f > "$(DIRNAME)/$$f"; done
	tar cvfz "$(DIRNAME).tgz" "$(DIRNAME)"

check:
	cd "$(DIRNAME)"
	make
	./autocc autos/shift.auto

DATE = $(shell date +"%y%m%d")
ARCHIVED_FILES = \
	code/*.s \
	parser.mly \
	lexer.mll \
	comp.ml
archive:
	tar cvfz archive-$(DATE).tgz $(ARCHIVED_FILES)

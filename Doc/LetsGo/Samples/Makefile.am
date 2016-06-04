
#
# Build the Go! sample programs for Lets Go!
#
include ${top_builddir}/go.Make

SUFFIXES = .go .gof .goc

GO_SRCS =  hello.go engine.go train.go scotsman.go directory.go lister.go publish.go party.go\
	protocol.go mail.go phemail.go ballroom.goc\
	meta.go dynamicRel.go dynamicRules.go parent.go partner.go ancestor.go family.go\
	strips.go blocks.go robbie.go

GO_TSTS = hello

GO_CODE = $(GO_SRCS:.go=.goc) 

EXTRA_DIST = ${GO_SRCS} 

CLEANFILES = ${GO_CODE} $(GO_SRCS:.go=.gof)

train.goc: engine.goc

scotsman.goc: engine.goc train.goc

protocol.goc: directory.goc 

party.goc: directory.goc lister.goc publish.goc

mail.goc: directory.goc protocol.goc

phemail.goc: directory.goc protocol.goc

ballroom.goc: protocol.goc mail.goc phemail.goc

parent.goc: meta.goc dynamicRel.goc

partner.goc: meta.goc dynamicRel.goc

family.goc: partner.goc meta.goc dynamicRel.goc dynamicRules.goc parent.goc partner.goc

dynamicRules.goc: meta.goc

ancestor.goc: meta.goc dynamicRules.goc parent.goc

family.goc: parent.goc partner.goc ancestor.goc meta.goc

metatest.goc: meta.goc parent.goc

robbie.goc: strips.goc

blocks.goc: strips.goc robbie.goc

all-local: ${GO_CODE}

check-local: ${GO_CODE}
	(for Pk in $(GO_TSTS); do\
	  GO_DIR=${top_builddir}/ ${top_builddir}/Engine/go ${TST_FLAGS} $${Pk}||exit 1; done)

install-data-local: ${GO_CODE}
	-${top_srcdir}/mkinstalldirs $(DESTDIR)${prefix}/Samples
	(for XX in ${GO_SRCS} ${GO_CODE}; do\
	  $(INSTALL_SCRIPT) $${XX} $(DESTDIR)${prefix}/Samples; done)



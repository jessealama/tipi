JAVA = java
XSLTXT = xsltxt.jar

all: paradox-interpretation.xsl

%.xsl: %.xsltxt
	$(JAVA) -jar $(XSLTXT) toXSL $*.xsltxt $*.xsl || rm $*.xsl;

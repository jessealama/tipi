JAVA = java
XSLTXT = xsltxt.jar

all: paradox-interpretation.xsl copy.xsl die.xsl trace.xsl paradox-diagram.xsl name-formulas.xsl

%.xsl: %.xsltxt
	$(JAVA) -jar $(XSLTXT) toXSL $*.xsltxt $*.xsl || rm $*.xsl;

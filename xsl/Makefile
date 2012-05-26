JAVA = java
XSLTXT = xsltxt.jar
CAT = cat

all: paradox-diagram.xsl

%.xsl: %.xsltxt
	which $(JAVA)
	$(JAVA) -jar $(XSLTXT) toXSL $*.xsltxt $*.xsl || rm $*.xsl;

clean:
	rm -f *.xsl

JAVA = java
XSLTXT = xsltxt.jar
CAT = cat


%.xsl: %.xsltxt
	$(JAVA) -jar $(XSLTXT) toXSL $*.xsltxt | sed -e 's/<!\-\- *\(<\/*xsl:document.*\) *\-\->/\1/g' > $*.xsl

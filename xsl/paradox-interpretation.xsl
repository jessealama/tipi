<?xml version='1.0' encoding='UTF-8'?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>

  <xsl:template match="/">
    <xsl:choose>
      <xsl:when test="tstp">
        <xsl:apply-templates select="tstp"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>Error: the required tstp document element is missing.</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tstp">
    <xsl:choose>
      <xsl:when test="formula[@name = &quot;domain&quot;]">
        <xsl:apply-templates select="formula[@name = &quot;domain&quot;]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>Error: the required domain formula is missing.</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="formula[@name = &quot;domain&quot;]">
    <xsl:choose>
      <xsl:when test="quantifier[@type = &quot;universal&quot;]">
        <xsl:text>Domain: </xsl:text>
        <xsl:for-each select="quantifier[@type = &quot;universal&quot;]">
          <xsl:for-each select="*[position() = last()]">
            <xsl:choose>
              <xsl:when test="self::disjunction">
                <xsl:for-each select="predicate[@name = &quot;=&quot;]">
                  <xsl:apply-templates select="." mode="emit-rhs"/>
                  <xsl:if test="position() &lt; last()">
                    <xsl:text>, </xsl:text>
                  </xsl:if>
                </xsl:for-each>
                <xsl:text>
</xsl:text>
              </xsl:when>
              <xsl:when test="self::predicate">
                <xsl:apply-templates select="." mode="emit-rhs"/>
                <xsl:text>
</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                <xsl:message terminate="yes">
                  <xsl:text>Error: unable to handle a domain formula whose matrix is neither an atom nor a disjunction.</xsl:text>
                </xsl:message>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>Error: the domain formula does not have the expected shape (universal quantification).</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="*" mode="emit-rhs">
    <xsl:choose>
      <xsl:when test="count (*) = 2">
        <xsl:apply-templates select="*[2]" mode="print"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>Error: we cannot print the right-hand side of an element that does not have exactly two children.</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="*" mode="print">
    <xsl:variable name="n" select="name (.)"/>
    <xsl:variable name="message" select="concat (&quot;Error: we managed to land in the default print template for a(n) &quot;, $n, &quot; element; something has gone awry.&quot;)"/>
    <xsl:message terminate="yes">
      <xsl:value-of select="$message"/>
    </xsl:message>
  </xsl:template>

  <xsl:template match="string[@name]" mode="print">
    <xsl:value-of select="@name"/>
  </xsl:template>
</xsl:stylesheet>

# assumes the existence of java and javac

CLASSPATH=lib/antlr-3.4-complete-no-antlrv2.jar:.

all: tex.xml

pascal3gLexer.java pascal3gParser.java  pascal3g.tokens: pascal3g.g
	java -cp ${CLASSPATH} org.antlr.Tool pascal3g.g

pascal3gLexer.class pascal3gParser.class: pascal3gLexer.java pascal3gParser.java
	javac -cp ${CLASSPATH} *.java

tex.xml: pascal3gLexer.class pascal3gParser.class
	java -cp ${CLASSPATH} MainPascal3g tex.p > tex.xml
	xmlstarlet val -e tex.xml

clean:
	rm *.class tex.xml

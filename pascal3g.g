// Pascal Grammar for ANTRL3
// See http://www.monperrus.net/martin/pascal-antlr3
//
// @author Hakki Dogusan
// @author Piet Schoutteten
// @author Terence Parr
// @author Martin Monperrus


grammar pascal3g;
options { output=AST; }

/*
// comment this for lower-case pascal
// and uncomment below 
tokens {
  AND              = 'AND'             ;
  ARRAY            = 'ARRAY'           ;
  BEGIN            = 'BEGIN'           ;
  BOOLEAN          = 'BOOLEAN'         ;
  CASE             = 'CASE'            ;
  CHAR             = 'CHAR'            ;
  CHR              = 'CHR'             ;
  EXIT              = 'EXIT'             ;
  CONST            = 'CONST'           ;
  DIV              = 'DIV'             ;
  DO               = 'DO'              ;
  DOWNTO           = 'DOWNTO'          ;
  ELSE             = 'ELSE'            ;
  END              = 'END'             ;
  FILE             = 'FILE'            ;
  FOR              = 'FOR'             ;
  FORWARD          = 'FORWARD'        ;
  FUNCTION         = 'FUNCTION'        ;
  GOTO             = 'GOTO'            ;
  IF               = 'IF'              ;
  IN               = 'IN'              ;
  INTEGER          = 'INTEGER'         ;
  LABEL            = 'LABEL'           ;
  MOD              = 'MOD'             ;
  NIL              = 'NIL'             ;
  NOT              = 'NOT'             ;
  OF               = 'OF'              ;
  OR               = 'OR'              ;
  PACKED           = 'PACKED'          ;
  PROCEDURE        = 'PROCEDURE'       ;
  PROGRAM          = 'PROGRAM'         ;
  REAL             = 'REAL'            ;
  RECORD           = 'RECORD'          ;
  REPEAT           = 'REPEAT'          ;
  SET              = 'SET'             ;
  THEN             = 'THEN'            ;
  TO               = 'TO'              ;
  TYPE             = 'TYPE'            ;
  UNTIL            = 'UNTIL'           ;
  VAR              = 'VAR'             ;
  WHILE            = 'WHILE'           ;
  WITH             = 'WITH'            ;
  UNIT             = 'UNIT'            ;
  INTERFACE        = 'INTERFACE'       ;
  USES             = 'USES'            ;
  STRING           = 'STRING'          ;
  IMPLEMENTATION   = 'IMPLEMENTATION'  ;
  BLOCK;    // list of statements
  IDLIST;   // list of identifiers
  ELIST;    // expression list for proc args etc...
  FUNC_CALL;// a call to a function
  PROC_CALL;// a call to a procedure
  SCALARTYPE; // IDLIST that is really a scalar type like (Mon,Tue,Wed)
  TYPELIST; // list of types such as for array declarations
  VARIANT_TAG;// for CASEs in a RECORD
  VARIANT_TAG_NO_ID;// for CASEs in a RECORD (no id, just a type)
  VARIANT_CASE;// a case of the variant
  CONSTLIST;  // List of constants
  FIELDLIST;  // list of fields in a record
  ARGDECLS; // overall group of declarations of args for proc/func.
  VARDECL;  // declaration of a variable
  ARGDECL;  // declaration of a parameter
  ARGLIST;  // list of actual arguments (expressions)
  TYPEDECL; // declaration of a type
  FIELD;    // the root a RECORD field
  SUBRANGE;    // a subrange type
  //----------------------------------------------------------------------------
  // OPERATORS
  //----------------------------------------------------------------------------
  PLUS            = '+'   ;
  MINUS           = '-'   ;
  STAR            = '*'   ;
  SLASH           = '/'   ;
  ASSIGN          = ':='  ;
  COMMA           = ','   ;
  SEMI            = ';'   ;
  COLON           = ':'   ;
  EQUAL           = '='   ;
  NOT_EQUAL       = '<>'  ;
  LT              = '<'   ;
  LE              = '<='  ;
  GE              = '>='  ;
  GT              = '>'   ;
  LPAREN          = '('   ;
  RPAREN          = ')'   ;
  LBRACK          = '['   ; // line_tab[line]
  LBRACK2         = '(.'  ; // line_tab(.line.)
  RBRACK          = ']'   ;
  RBRACK2         = '.)'  ;
  POINTER         = '^'   ;
  AT              = '@'   ;
  DOT             = '.' ;
  DOTDOT          = '..';
  LCURLY          = '{' ;
  RCURLY          = '}' ;
}
*/

// version with lower case keywords
tokens {
  AND              = 'and'             ;
  ARRAY            = 'array'           ;
  BEGIN            = 'begin'           ;
  BOOLEAN          = 'boolean'         ;
  CASE             = 'case'            ;
  CHAR             = 'char'            ;
  CHR              = 'chr'             ;
  EXIT             = 'exit'            ;
  CONST            = 'const'           ;
  DIV              = 'div'             ;
  DO               = 'do'              ;
  DOWNTO           = 'downto'          ;
  ELSE             = 'else'            ;
  END              = 'end'             ;
  FILE             = 'file'            ;
  FOR              = 'for'             ;
  FORWARD          = 'forward'        ;
  FUNCTION         = 'function'        ;
  GOTO             = 'goto'            ;
  IF               = 'if'              ;
  IN               = 'in'              ;
  INTEGER          = 'integer'         ;
  LABEL            = 'label'           ;
  MOD              = 'mod'             ;
  NIL              = 'nil'             ;
  NOT              = 'not'             ;
  OF               = 'of'              ;
  OR               = 'or'              ;
  PACKED           = 'packed'          ;
  PROCEDURE        = 'procedure'       ;
  PROGRAM          = 'program'         ;
  REAL             = 'real'            ;
  RECORD           = 'record'          ;
  REPEAT           = 'repeat'          ;
  SET              = 'set'             ;
  THEN             = 'then'            ;
  TO               = 'to'              ;
  TYPE             = 'type'            ;
  UNTIL            = 'until'           ;
  VAR              = 'var'             ;
  WHILE            = 'while'           ;
  WITH             = 'with'            ;
  UNIT             = 'unit'            ;
  INTERFACE        = 'interface'       ;
  USES             = 'uses'            ;
  STRING           = 'string'          ;
  IMPLEMENTATION   = 'implementation'  ;
  BLOCK;    // list of statements
  IDLIST;   // list of identifiers
  ELIST;    // expression list for proc args etc...
  FUNC_CALL;// a call to a function
  PROC_CALL;// a call to a procedure
  SCALARTYPE; // IDLIST that is really a scalar type like (Mon,Tue,Wed)
  TYPELIST; // list of types such as for array declarations
  VARIANT_TAG;// for CASEs in a RECORD
  VARIANT_TAG_NO_ID;// for CASEs in a RECORD (no id, just a type)
  VARIANT_CASE;// a case of the variant
  CONSTLIST;  // List of constants
  FIELDLIST;  // list of fields in a record
  ARGDECLS; // overall group of declarations of args for proc/func.
  VARDECL;  // declaration of a variable
  ARGDECL;  // declaration of a parameter
  ARGLIST;  // list of actual arguments (expressions)
  TYPEDECL; // declaration of a type
  FIELD;    // the root a RECORD field
  SUBRANGE;    // a subrange type
  //----------------------------------------------------------------------------
  // OPERATORS
  //----------------------------------------------------------------------------
  PLUS            = '+'   ;
  MINUS           = '-'   ;
  STAR            = '*'   ;
  SLASH           = '/'   ;
  ASSIGN          = ':='  ;
  COMMA           = ','   ;
  SEMI            = ';'   ;
  COLON           = ':'   ;
  EQUAL           = '='   ;
  NOT_EQUAL       = '<>'  ;
  LT              = '<'   ;
  LE              = '<='  ;
  GE              = '>='  ;
  GT              = '>'   ;
  LPAREN          = '('   ;
  RPAREN          = ')'   ;
  LBRACK          = '['   ; // line_tab[line]
  LBRACK2         = '(.'  ; // line_tab(.line.)
  RBRACK          = ']'   ;
  RBRACK2         = '.)'  ;
  POINTER         = '^'   ;
  AT              = '@'   ;
  DOT             = '.' ;
  DOTDOT          = '..';
  LCURLY          = '{' ;
  RCURLY          = '}' ;
}



program
    : programHeading (INTERFACE)?
      block
      DOT
      -> ^(PROGRAM programHeading block)
    ;

programHeading
    : PROGRAM! identifier (LPAREN! identifierList RPAREN!)? SEMI!
    | UNIT identifier SEMI!
  ;

identifier
    : IDENT
    ;

block
    : ( labelDeclarationPart
      | constantDefinitionPart
      | typeDefinitionPart
      | variableDeclarationPart
      | procedureAndFunctionDeclarationPart
      | usesUnitsPart
      | IMPLEMENTATION
      )*
      compoundStatement
    ;

usesUnitsPart
    : USES^ identifierList SEMI!
    ;

labelDeclarationPart
    : LABEL^ label ( COMMA! label )* SEMI!
    ;

label
    : unsignedInteger
    ;

constantDefinitionPart
    : CONST^ constantDefinition ( SEMI! constantDefinition )* SEMI!
    ;

constantDefinition
    : identifier EQUAL^ constant
    ;

constantChr
    : CHR^ LPAREN! (unsignedInteger|identifier) RPAREN!
    ;

constant
    : unsignedNumber
    | s=sign n=unsignedNumber -> ^($s $n) 
    | identifier
    | s2=sign id=identifier -> ^($s2 $id) 
    | string
    | constantChr
    ;

unsignedNumber
    : unsignedInteger
    | unsignedReal
    ;

unsignedInteger
    : NUM_INT
    ;

unsignedReal
    : NUM_REAL
    ;

sign
    : PLUS | MINUS
    ;

string
    : STRING_LITERAL
    ;

typeDefinitionPart
    : TYPE^ typeDefinition ( SEMI! typeDefinition )* SEMI!
    ;

//PSPSPS
typeDefinition
    : identifier e=EQUAL^ {$e.setType(TYPEDECL);}
      ( type
      | functionType 
      | procedureType
      )
    ;

functionType
    : FUNCTION^ (formalParameterList)? COLON! resultType
    ;

procedureType
    : PROCEDURE^ (formalParameterList)?
    ;

type
    : simpleType
    | structuredType
    | pointerType
    ;

simpleType
    : scalarType
    | subrangeType 
    | typeIdentifier
    | stringtype
    ;

scalarType
    : LPAREN identifierList RPAREN 
    -> ^(SCALARTYPE identifierList)
    ;

subrangeType
    : c1=constant DOTDOT c2=constant
    -> ^(SUBRANGE $c1 $c2)
    ;

typeIdentifier
    : identifier
    | CHAR
    | BOOLEAN
    | INTEGER
    | REAL
    | STRING // as in return type: FUNCTION ... (...): string;//  clashes with strinType
    ;

structuredType
    : PACKED^ unpackedStructuredType
   | unpackedStructuredType
    ;

unpackedStructuredType
    : arrayType
    | recordType
    | setType
    | fileType
    | subrangeType // to allow "packed 1..255", but raises warnings.
    ;

stringtype
    : STRING^ LPAREN! (identifier|unsignedNumber) RPAREN! // string(3)
    | STRING^ LBRACK2! (identifier|unsignedNumber) RBRACK2! // string[3]
    ;

arrayType
    : ARRAY^ LBRACK! typeList RBRACK! OF! componentType
    | ARRAY^ LBRACK2! typeList RBRACK2! OF! componentType
  ;

typeList
  : indexType ( COMMA indexType )*
  -> ^(TYPELIST indexType+)
  ;

indexType
    : simpleType
    ;

componentType
    : type
    ;

recordType
    : RECORD^ fieldList END!
    ;

fieldList
    : ( f1=fixedPart ( SEMI f2=variantPart | SEMI )?
      | f3=variantPart
      )
      -> ^(FIELDLIST $f1? $f2? $f3?)
    ;

fixedPart
    : recordSection ( SEMI! recordSection )*
    ;

recordSection
    : identifierList COLON type
    -> ^(FIELD identifierList type)
    ;

variantPart
    : CASE^ tag OF! variant ( SEMI! variant | SEMI! )*
    ;

tag
    : id=identifier COLON t=typeIdentifier -> ^(VARIANT_TAG $id $t) 
    | t2=typeIdentifier  -> ^(VARIANT_TAG_NO_ID $t2)
    ;

variant
    : constList c=COLON^ {$c.setType(VARIANT_CASE);}
    LPAREN! fieldList RPAREN!
    ;

setType
    : SET^ OF! simpleType
    ;

fileType
    : FILE^ OF! type
    | FILE
    ;

pointerType
    : POINTER^ typeIdentifier
    ;

// Yields a list of VARDECL-rooted subtrees with VAR at the overall root 
variableDeclarationPart
    : VAR^ variableDeclaration ( SEMI! variableDeclaration )* SEMI!
    ;

variableDeclaration
    : identifierList c=COLON^ {$c.setType(VARDECL);} type
    ;

procedureAndFunctionDeclarationPart
    : procedureOrFunctionDeclaration SEMI!
    ;

procedureOrFunctionDeclaration
    : procedureDeclaration
    | functionDeclaration
    ;

procedureDeclaration
    : PROCEDURE^ identifier (formalParameterList)? SEMI!
      ( block | directive )
    ;

// http://www.felix-colibri.com/papers/compilers/pascal_grammar/pascal_grammar.html
// function_declaration= function_heading ';' ( block | directive ) .
// function_heading= FUNCTION NAME [ formal_parameter_list ] ':' result_type .
// directive= FORWARD .
functionDeclaration
    : FUNCTION^ identifier (formalParameterList)? COLON! resultType SEMI!
      ( block | directive )
    ;

directive
    : FORWARD
    ;


formalParameterList
    : LPAREN formalParameterSection ( SEMI formalParameterSection )* RPAREN
    -> ^(ARGDECLS formalParameterSection+)
    ;

formalParameterSection
    : parameterGroup
    | VAR^ parameterGroup
    | FUNCTION^ parameterGroup
    | PROCEDURE^ parameterGroup
    ;

parameterGroup
    : ids=identifierList COLON t=typeIdentifier
    -> ^(ARGDECL identifierList typeIdentifier)
    ;

identifierList
    : identifier ( COMMA identifier )*
    ->^(IDLIST identifier+)
    ;

constList
    : constant ( COMMA constant )*
    ->^(CONSTLIST constant+)
    ;

resultType
    : typeIdentifier
    ;

statement
    : label COLON^ unlabelledStatement
    | unlabelledStatement
    ;

unlabelledStatement
    : simpleStatement
    | structuredStatement
    ;

exitStatement
    : EXIT^ 
    ;


simpleStatement
    : assignmentStatement
    | procedureStatement
    | exitStatement
    | gotoStatement
    | emptyStatement
    ;

assignmentStatement
    : variable ASSIGN^ expression
    ;

variable
    : ( AT^ identifier // AT is root of identifier; then other op becomes root
      | identifier
      )
      (  LBRACK^ expression ( COMMA! expression)* RBRACK!
      | LBRACK2^ expression ( COMMA! expression)* RBRACK2!
      | DOT^ identifier
      | POINTER^
      )*
    ;

expression
    : simpleExpression
    ( (EQUAL^ | NOT_EQUAL^ | LT^ | LE^ | GE^ | GT^ | IN^) simpleExpression )*
    ;

simpleExpression
    : term ( (PLUS^ | MINUS^ | OR^) term )*
    ;

term
  : signedFactor ( (STAR^ | SLASH^ | DIV^ | MOD^ | AND^) signedFactor )*
    ;

signedFactor
    : (PLUS^|MINUS^)? factor
    ;

factor
    : variable
    | LPAREN! expression RPAREN!
    | functionDesignator
    | unsignedConstant
    | set
    | NOT^ factor
    ;

unsignedConstant
    : unsignedNumber
    | constantChr         //pspsps added
    | string
    | NIL
    ;

functionDesignator
    : id=identifier LPAREN args=parameterList RPAREN
    -> ^(FUNC_CALL $id $args) 
    ;

parameterList
    : actualParameter ( COMMA actualParameter )*
    -> ^(ARGLIST actualParameter+)
    ;

set
    : LBRACK elementList RBRACK  -> ^(SET elementList?) // elementList may be empty
    | LBRACK2 elementList RBRACK2 -> ^(SET elementList?)
    ;

elementList
    : element ( COMMA! element )*
    |
    ;

element
    : expression ( DOTDOT^ expression )?
    ;

procedureStatement
    : id=identifier ( LPAREN args=parameterList RPAREN )?
    -> ^(PROC_CALL identifier parameterList?)
    ;

actualParameter
    // in tex one encounters WRITE(LOGFILE,' ',STRPTR-INITSTRPTR:1,' string')
    : expression (COLON unsignedInteger)?
    ;

gotoStatement
    : GOTO^ label
    ;

emptyStatement
    :
    ;

empty
    : // nothing
    ;

structuredStatement
    : compoundStatement
    | conditionalStatement
    | repetetiveStatement
    | withStatement
    ;

compoundStatement
    : BEGIN
    statements
      END
        -> ^(BLOCK statements*) // for the AST
    ;

statements
    : statement ( SEMI! statement )* 
    ;

conditionalStatement
    : ifStatement
    | caseStatement
    ;

ifStatement
    : IF^ expression THEN! statement
      (
    // CONFLICT: the old "dangling-else" problem...
    //           ANTLR generates proper code matching
    //       as soon as possible.  Hush warning.
     ELSE! statement
    )?
    ;

caseStatement 
    : CASE^ expression OF!
        caseListElement ( SEMI! caseListElement )* SEMI!?
      ( ELSE! statements )?
      END!
    ;

caseListElement
    : constList COLON^ statement
    ;

repetetiveStatement
    : whileStatement
    | repeatStatement
    | forStatement
    ;

whileStatement
    : WHILE^ expression DO! statement
    ;

repeatStatement
    : REPEAT^ statements UNTIL! expression
    ;

forStatement
    : FOR^ identifier ASSIGN! forList DO! statement
    ;

forList
    : initialValue (TO^ | DOWNTO^) finalValue
    ;

initialValue
    : expression
    ;

finalValue
    : expression
    ;

withStatement
    : WITH^ recordVariableList DO! statement
    ;

recordVariableList
    : variable ( COMMA! variable )*
    ;

// Whitespace -- ignored
WS      : ( ' '
    |  '\t'
    |  '\f'
    // handle newlines
    |  (  '\r\n'  // Evil DOS
      |  '\r'    // Macintosh
      |  '\n'    // Unix (the right way)
      )
      {  }
    )
    { $channel=HIDDEN; }
  ;

COMMENT_1
        :   '(*'
       ( 
         // ?=> Gated semantic predicate
         // http://stackoverflow.com/questions/3056441/what-is-a-semantic-predicate-in-antlr
         { input.LA(2) != ')' }?=> '*'
           |   ~('*') 
       )*
          '*)'  
    {$channel=HIDDEN; }
  ;

COMMENT_2
        :  '{'
        (  ~('}') )*
           '}'
    {$channel=HIDDEN; }
  ;

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
IDENT  :  ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*   
  ;

// string literals
STRING_LITERAL
  : '\'' ('\'\'' | ~('\''))* '\''   //pspsps   * in stead of + because of e.g. ''
  ;

// a numeric literal.  Form is (from Wirth)
//  digits
//  digits . digits
//  digits . digits exponent
//  digits exponent
//
NUM_INT
  : ('0'..'9')+ // everything starts with a digit sequence
    ( ( {(input.LA(2)!='.')&&(input.LA(2)!=')')}?=>       // force k=2; avoid ".."
//PSPSPS example ARRAY (.1..99.) OF char; // after .. thinks it's a NUM_REAL
        '.' {$type = NUM_REAL;} // dot means we are float
        ('0'..'9')+ (EXPONENT)?
      )?
    | EXPONENT {$type = NUM_REAL;}  // 'E' means we are float
    )
  ;

// a couple protected methods to assist in matching floating point numbers
// Sometimes you will need some help or rules to make your lexer grammar more readable. Use the fragment modifier in front of the rule
fragment
EXPONENT
  :  ('e') ('+'|'-')? ('0'..'9')+
  ;

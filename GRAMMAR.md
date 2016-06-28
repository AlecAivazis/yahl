# Grammar

This document shows the full grammar of yahl.

```
<int>          ::= <int>
<float>        ::= <float>
<string>       ::= <string>
<function>     ::= <name> <callArgList> -> <expression>
<lambda>       ::= <callArgList> -> <expression>
<call>         ::= <name> <expr>|<name>
<callArgList>  ::= <name> <callArgList>|<name>
<indentifier>  ::= <name>|<name>
```
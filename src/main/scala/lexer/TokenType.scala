package lexer

enum TokenType:
  case NUMBER, ID, STRING, VAR, VAL

  case PRINT
  case DEF

  case IF, ELSE
  case WHILE

  case PLUS, MINUS, STAR, SLASH
  case EQ, EQEQ, NEQ, EXCL
  case LT, GT, LTEQ, GTEQ
  case AND, OR

  case LPAREN, RPAREN
  case LBRACE, RBRACE
  case SEMICOLON

  case EOF
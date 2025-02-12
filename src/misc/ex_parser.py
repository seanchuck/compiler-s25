"""
IMPLEMENT ERROR HANDLING
"""


from ex_scanner import *


class Params:
    pass

class Vars:
    pass
class Block:
    pass

class Funcdef:
    name: Identifier
    params: Params
    functype: Type | None
    block : Block



class Program:
    funcdefs : Funcdef



class Parser:
    tokens: list[Token]

    def __init__(self, tokens):
        self.tokens = tokens

    def peek(self) -> str | None:
        return self.tokens[0]

    def gobble(self, ntoken: int) ->str | None:
        if not type(self.tokens[0]) == EOF:
            token = self.tokens[0]
            self.tokens = self.tokens[1:]
            return token
        

    """"""
    def expect(self, expected: Token, received: Token) -> Token:
        assert(type(received) == expected, "Failed type expectation")
        return received
        

    def parse(self) -> Program:
        funcdefs = []
        while type(self.peek()) != EOF:
            funcdefs.append(self.parse_funcdefs())

        return Program(funcdefs)

    def parse_funcdef(self) -> Funcdef:
        pass

    def parse_params(self) -> Params:
        pass

    def parse_statement(self) -> Statement:
        pass

    def parse_expression(self) -> Expression:
        # 3*5 + 7
        # Order of operations!
        pass


    def parse_nonbin_expr(self):
        # if we see a number: parse a literal:
        # if we see an identifier: wait for te paren to determine 
        # whether it's a function call
        pass


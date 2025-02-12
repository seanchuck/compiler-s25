"""
1. Write type system for elements of grammar
2. Define precedence: keyword --> identifier

"""

# Use enum for Rust -- Treelike structure: Token --> Keyword, symbol --> 

# enum token { OpenParen, Int, ...}
class Token:
    pass

class Keyword(Token):
    pass

class Symbol(Token):
    pass

class Func(Keyword):
    pass

class Return(Keyword):
    pass

class Type(Keyword):
    pass


class OpenParen(Symbol):
    pass

class CloseParen(Symbol):
    pass

class Eq(Symbol):
    pass


class Int(Token):
    pass

class Int64(Token):
    pass


class EOF(token):
    pass

class Identifier(Token):
    value: str
    
    def __init__(self, value: str):
        self.value = value

class Literal(Token):
    value: str
    
    def __init__(self, value: str):
        self.value = value



class Scanner:
    rest: str
    def __init__(self, input: str):
        # Portion of program yet to be scanned
        self.rest = input

    def is_end_of_file(self) -> bool:
        return len(self.rest) == 0


    def peek(self) -> str | None:
        if not self.is_end_of_file():
            return self.rest[0]

    def gobble(self, ntoken: int) ->str | None:
        if not self.is_end_of_file():
            self.rest = self.rest[ntoken:]

    def take_while(self, pred):
        i = 0

        # Go until find a whitespace
        while i < len(self.rest) and pred(self.rest[i]):
            i += 1
        return self.rest[:i]
    

    # symbols NOT prefix-free (i.e., +=)
    symbol_to_token = [
        ("==", Eq()),
        ("(", OpenParen),
        (")", CloseParen)
    ]

    # keywords typically prefix-free
    keyword_to_token = {
        "func": Func(),
        "if": If(),
    }
    
    # "Tape roll" that gobbles up tokens at a time
    def lex(self) -> list[Token]:
        lexed = []
        while not self.is_end_of_file():
            if self.peek().isspace():
                self.consume(1)
                continue

            found_symbol = False
            for symbol, token in symbol_to_token:
                if self.rest.startswith(symbol) and not found_symbol:
                    self.consume(len(symbol))
                    lexed.append(token)
                    found_symbol = True
            
            if found_symbol:
                continue

            # Lex keywords and IDs
            if self.peek().isalpha():
                keyword_or_id = self.take_while(lambda x : x.isalnum())
                self.gobble(len(keyword_or_id))
                if keyword := keyword_to_token.get(keyword_or_id):
                    lexed.append(keyword)
                else:
                    lexed.append(Identifier(keyword_or_id))

            # Lex literals
            elif self.peek().isnumeric():
                literal = self.take_while(lambda x: x.isnumeric())

            else:
                assert False, "unrecognized symbol"












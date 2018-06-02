extern crate regex;

use regex::Regex;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct TokenKind(pub u32);

pub const ERROR: TokenKind = TokenKind(0);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

pub type ExternRule = Box<Fn(&str) -> Option<usize> + Send + Sync>;

pub struct Lexer {
    rules: Vec<Rule>,
}

pub struct Rule {
    kind: TokenKind,
    re: Regex,
    f: Option<ExternRule>,
}

pub struct LexerBuilder {
    rules: Vec<Rule>,
}

impl Lexer {
    pub fn next_token(&self, input: &str) -> Token {
        assert!(!input.is_empty(), "next_token should not be called with empty input");
        self.valid_token(input).unwrap_or_else(|| {
            self.invalid_token(input)
        });
        let first_char_len = match input.chars().next() {
            Some(c) => c.len_utf8(),
            None => panic!()
        };

        let longest_match = self.rules.iter().rev()
            .filter_map(|rule| {
                let m = rule.re.find(input)?;
                Some((m.end(), rule))
            })
            .max_by_key(|&(len, _)| len);

        let (len, kind, f) = match longest_match {
            Some((len, rule)) => (len, rule.kind, &rule.f),
            None => return Token { kind: ERROR, len: first_char_len },
        };

        assert!(len > 0, "token {:?} has length zero", kind);

        match f {
            None => Token { kind, len },
            Some(f) => if let Some(len) = f(input) {
                assert!(len > 0, "external token {:?} has length zero", kind);
                Token { kind, len }
            } else {
                Token { kind: ERROR, len: first_char_len }
            }
        }
    }

    pub fn tokenize(&self, input: &str) -> Vec<Token> {
        let mut ret = Vec::new();
        let mut rest = input;
        while !rest.is_empty() {
            let tok = self.next_token(rest);
            ret.push(tok);
            assert!(tok.len > 0);
            rest = &rest[tok.len..];
        }
        ret
    }

    pub fn test(&self, input: &str, expected: &str) {
        let mut len = 0;
        let mut actual = String::new();

        for t in self.tokenize(input) {
            let end = len + t.len;
            let text = &input[len..end];
            actual += &format!("{:?} {}\n", text, t.kind.0);
            len = end;
        }
        let expected = expected.trim();
        let actual = actual.trim();

        assert_eq!(
            expected, actual,
            "\nExpected:\n\n\
            {}\n\n\
            Actual:\n\n\
            {}\n\n",
            expected, actual,
        );
    }

    fn valid_token(&self, input: &str) -> Option<(Token)> {
        let longest_match = self.rules.iter().rev()
            .filter_map(|rule| {
                let m = rule.re.find(input)?;
                Some((m.end(), rule))
            })
            .max_by_key(|&(len, _)| len)?;

        let (len, rule) = longest_match;
        let len = match &rule.f {
            Some(f) => f(input)?,
            None => len,
        };
        assert!(len > 0, "empty token\n\
                          kind: {:?}\n\
                          re: {:?}\n\
                          input {:?}", rule.kind, rule.re, input);
        Some(Token { kind: rule.kind, len })
    }

    fn invalid_token(&self, input: &str) -> Token {
        let mut len = 0;
        for c in input.chars() {
            len += c.len_utf8();
            if self.valid_token(&input[len..]).is_some() {
                break;
            }
        }
        assert!(0 < len && len <= input.len());
        Token { kind: ERROR, len }
    }
}

impl LexerBuilder {
    pub fn new() -> LexerBuilder {
        LexerBuilder { rules: Vec::new() }
    }

    pub fn token(&mut self, kind: TokenKind, re: &str) {
        self.rule(kind, re, None);
    }

    pub fn tokens(&mut self, rules: &[(TokenKind, &str)]) {
        for &(kind, re) in rules {
            self.token(kind, re)
        }
    }

    pub fn external_token<F>(&mut self, kind: TokenKind, re: &str, f: F)
        where F: Fn(&str) -> Option<usize> + 'static + Send + Sync
    {
        self.rule(kind, re, Some(Box::new(f)))
    }

    pub fn rule(&mut self, kind: TokenKind, re: &str, f: Option<ExternRule>) {
        let rule = Rule { kind, re: parse_re(re), f };
        self.rules.push(rule);
    }

    pub fn build(self) -> Lexer {
        Lexer { rules: self.rules }
    }
}

fn parse_re(re: &str) -> Regex {
    Regex::new(&format!("^({})", re)).unwrap()
}


#[test]
fn tokenize_longest_first_wins() {
    const WS: TokenKind = TokenKind(1);
    const FOO: TokenKind = TokenKind(10);
    const WORD: TokenKind = TokenKind(11);
    const FOOBAR: TokenKind = TokenKind(12);

    let lexer = {
        let mut builder = LexerBuilder::new();
        builder.tokens(&[
            (WS, r"\s+"),
            (FOO, "foo"),
            (WORD, r"\w+"),
            (FOOBAR, "foobar"),
        ]);
        builder.build()
    };
    lexer.test(
        "foo foob foobar",
        r#"
"foo" 10
" " 1
"foob" 11
" " 1
"foobar" 11"#,
    );
}

#[test]
fn extern_rule() {
    const WS: TokenKind = TokenKind(1);
    const WORD: TokenKind = TokenKind(2);
    const COMMENT: TokenKind = TokenKind(3);

    let lexer = {
        let mut builder = LexerBuilder::new();
        builder.tokens(&[
            (WS, r"\s+"),
            (WORD, r"\w+"),
        ]);
        builder.rule(COMMENT, r"\(\*", Some(Box::new(|input| nested_comment(input))));
        builder.build()
    };

    lexer.test(
        "foo (* (* bar *) *) baz *) (*",
        r#"
"foo" 2
" " 1
"(* (* bar *) *)" 3
" " 1
"baz" 2
" " 1
"*" 0
")" 0
" " 1
"(*" 3"#
    );

    fn nested_comment(input: &str) -> Option<usize> {
        assert!(input.starts_with("(*"));
        let mut level = 0;
        let mut len = 0;

        let mut rest = input;
        loop {
            let bite = match rest.chars().next() {
                None => return Some(len),
                _ if rest.starts_with("(*") => {
                    level += 1;
                    2
                }
                _ if rest.starts_with("*)") => {
                    level -= 1;
                    2
                }
                Some(c) => c.len_utf8(),
            };
            len += bite;
            rest = &rest[bite..];
            if level == 0 {
                return Some(len);
            }
        }
    }
}

module Pretty where
newtype Doc = Doc [String]

instance Show Doc where
    show (Doc ls) = unlines ls

instance Monoid Doc where
    mempty = Doc []
    mappend = (<++>)

(<++>) :: Doc -> Doc -> Doc -- append lines to the Doc
(Doc a) <++> (Doc b) = Doc (a ++ b)

(<>) :: Doc -> Doc -> Doc -- append to the last line of the Doc
(Doc [])     <> s = mempty                                -- no line to add to
(Doc [l])    <> (Doc []) = Doc [l]                        -- no doc to add
(Doc [l])    <> (Doc (s:ss)) = Doc ([l ++ s]) <++> Doc ss -- join the newly adjacent lines
(Doc (l:ls)) <> s = Doc [l] <++> ((Doc ls) <> s)

(<+>) :: Doc -> Doc -> Doc -- append to the end of the Doc
(Doc [])     <+> dss = dss
(Doc [l])    <+> (Doc []) = Doc ([l])                      -- no doc to add
(Doc [l])    <+> (Doc (s:ss)) = Doc ([l ++ s]) <++> Doc ss -- join the newly adjacent lines
(Doc (l:ls)) <+> dss = Doc [l] <++> (Doc ls <+> dss)

tab :: Int -> Doc -> Doc --TODO allow indentation to be erased when <>ing lines
tab n (Doc ls) = Doc $ map (take n (repeat '\t') ++) ls

indent :: Int -> Doc -> Doc
indent n (Doc ls) = Doc $ map (take n (repeat ' ') ++) ls

char :: Char -> Doc
char s = Doc [[s]]

string :: String -> Doc
string s = Doc [s]
-- string a <> string b == string $ a ++ b

int :: Int -> Doc
int n = string $ show n

float :: Float -> Doc
float n = string $ show n

plural :: Int -> Doc -> Doc -> Doc
plural 1 s _ = s
plural _ _ p = p

thing :: Int -> Doc -> Doc -> Doc
thing n s p = int n <> space <> plural n s p

comma  = char ','
semi   = char ';'
colon  = char ':'
space  = char ' '
equals = char '='
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
lbrace = char '{'
rbrace = char '}'
quote  = char '\''
doubleQuote = char '"'

-- horizontal (inline) wrapping
hsurround :: Doc -> Doc -> Doc -> Doc
hsurround l r d = l <+> d <+> r
hparens = hsurround lparen rparen
hbracks = hsurround lbrack rbrack
hbraces = hsurround lbrace rbrace
hquotes = hsurround quote quote
hdoubleQuotes = hsurround doubleQuote doubleQuote
-- vertical line wrapping
vsurround :: Doc -> Doc -> Doc -> Doc
vsurround l r d = l <++> d <++> r
vparens = vsurround lparen rparen
vbracks = vsurround lbrack rbrack
vbraces = vsurround lbrace rbrace
vquotes = vsurround quote quote
vdoubleQuotes = vsurround doubleQuote doubleQuote

-- code block wrapping
wrapTBlock :: Doc -> Doc -> Doc -> Doc
wrapTBlock l r d = l <++> tab 1 d <++> r
vTBlock :: Doc -> Doc
vTBlock = wrapTBlock lbrace rbrace

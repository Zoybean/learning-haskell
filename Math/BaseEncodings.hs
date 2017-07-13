module BaseEncodings where
import Base (Base, BaseB (..), BaseU (..), Sign (..))

-- Existing bases --
-- notation used:
-- sbx = signed base x
-- ubx = unsigned base x
-- bbx = balanced base x

-- unsigned base 2 -- binary
ub2 = BaseU Unsigned "01"

-- base 3 -- ternary
sb3 = BaseU (Signed '-') ['0'..'2']
ub3 = BaseU Unsigned ['0'..'2']
-- balanced base 3 -- balanced ternary
bb3  = BaseB '0' [('T','1')] -- visual, ligature -1
bb3a = BaseB '-' [('┬','┴')] -- visual, pointing up / down
bb3b = BaseB '+' [('L','Γ')] -- visual, marked top / bottom

-- base 4 -- quaternary
sb4 = BaseU (Signed '-') ['0'..'3']
ub4 = BaseU Unsigned ['0'..'3']

-- base 5 -- quiniary
sb5 = BaseU (Signed '-') ['0'..'4']
ub5 = BaseU Unsigned ['0'..'4']

-- base 6 -- senary
sb6 = BaseU (Signed '-') ['0'..'5']
ub6 = BaseU Unsigned ['0'..'5']

-- base 8 -- octal
sb8 = BaseU (Signed '-') ['0'..'7']
ub8 = BaseU Unsigned ['0'..'7']

-- base 10 -- decimal
ub10 = BaseU Unsigned ['0'..'9']
sb10 = BaseU (Signed '-') ['0'..'9']

-- base 12 -- duodecimal / dozenal
-- source: wikipedia: https://en.wikipedia.org/wiki/Duodecimal#Symbols
-- dozenal society of great britain
ub12dgb = BaseU Unsigned     $ ['0'..'9'] ++ ['ᘔ','Ɛ']
sb12dgb = BaseU (Signed '-') $ ['0'..'9'] ++ ['ᘔ','Ɛ']
-- dozenal society of america
ub12dus = BaseU Unsigned     $ ['0'..'9'] ++ ['X','Ɛ']
sb12dus = BaseU (Signed '-') $ ['0'..'9'] ++ ['X','Ɛ']

-- base 16 -- hexadecimal
-- source: wikipedia: https://en.wikipedia.org/wiki/Hexadecimal
-- upper
ub16u = BaseU Unsigned     $ ['0'..'9'] ++ ['A'..'F']
sb16u = BaseU (Signed '-') $ ['0'..'9'] ++ ['A'..'F']
-- lower
ub16l = BaseU Unsigned     $ ['0'..'9'] ++ ['a'..'f']
sb16l = BaseU (Signed '-') $ ['0'..'9'] ++ ['a'..'f']

-- base 20 -- vigesimal
-- source: wikipedia: https://en.wikipedia.org/wiki/Vigesimal#Places
-- contiguous
ub20 = BaseU Unsigned     $ ['0'..'9'] ++ ['A'..'J']
sb20 = BaseU (Signed '-') $ ['0'..'9'] ++ ['A'..'J']
-- without 'I'
ub20' = BaseU Unsigned     $ ['0'..'9'] ++ ['A'..'H'] ++ ['J','K']
sb20' = BaseU (Signed '-') $ ['0'..'9'] ++ ['A'..'H'] ++ ['J','K']

-- base 36 -- hexatrigesimal
ub36 = BaseU Unsigned     $ ['0'..'9'] ++ ['A'..'Z']
sb36 = BaseU (Signed '-') $ ['0'..'9'] ++ ['A'..'Z']

-- unsigned base 58
-- source: wikipedia: https://en.wikipedia.org/wiki/Base58
-- bitcoin addresses
ub58bitc = BaseU Unsigned $ ['1'..'9'] ++ "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
-- IPFS hashes
ub58ipfs = ub58bitc
-- ripple addresses
ub58ripl = BaseU Unsigned $ "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
-- flickr short url encoding
ub58flkr = BaseU Unsigned $ ['1'..'9'] ++ "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

-- unsigned base 62
-- source: wikipedia: https://de.wikipedia.org/wiki/Base62
ub62 = BaseU Unsigned $ ['A'..'Z'] ++ ['0'..'9'] ++ ['a'..'z']

-- unsigned base 64
-- source: wikipedia: https://en.wikipedia.org/wiki/Base64#Design
ub64mime = BaseU Unsigned $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

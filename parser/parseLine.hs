-- parse a sgml line

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

--import Text.ParserCombinators.Parsec

--sgmlFile = endBy sgmlLn eol
--sgmlLn   = field `sepBy` (many space)
--field    = many (noneOf " \n")
----eol      = char "\n"


--data SgmlLine = SgmlLine {
--    getTag    :: String
--  , getValue  :: String
-- } deriving (Show, Eq, Ord)

tag :: Parsec.Parsec String () String
tag = do
      Parsec.char '<'
      content <- many (Parsec.noneOf ">")
      Parsec.char '>'
      return content

value :: Parsec.Parsec String () String
value = do
        content <- many Parsec.anyChar
        return content

--sgmlLine :: Parser SgmlLine
--sgmlLine = do
--           t <- tag 
--           many Parsec.space
--           v <- value 
--           return $ SgmlLine t v 

--testLine = "<TITLE> Kalevala"

--main = case parse SgmlLine "(test)" testLine of
--            Left err  -> print err
------            Right res -> print res

--parseSGML :: Parser String () (String, String)
--parseSGML input = parse sgmlFile "(unknown)" input


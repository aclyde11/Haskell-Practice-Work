import Control.Monad.State
import Prelude hiding (head)

type HTML = State (Int, String)

render :: HTML a -> String
render mHTML = snd $  execState mHTML (0, "")

adjustDepth :: (Int -> Int) -> HTML ()
adjustDepth f = modify $ \(k, s) -> (f k, s)

incDepth, decDepth :: HTML ()
incDepth = adjustDepth (+1)
decDepth = adjustDepth (subtract 1)

tab :: HTML ()
tab = string "    "

n_tabs :: Int -> HTML ()
n_tabs n = replicateM_ n tab

newline :: HTML ()
newline = string ['\n']

string :: String -> HTML ()
string s = modify $ fmap (++s)

indentedString :: String -> HTML ()
indentedString s = do
  (k, _) <- get
  n_tabs k
  string s
  newline

tag :: String -> HTML a -> HTML ()
tag t mHTML = do
  indentedString $ "<" ++ t ++ ">"
  incDepth
  mHTML
  decDepth
  indentedString $ "</" ++ t ++ ">"

html  = tag "html"
head  = tag "head"
title = tag "title"
body  = tag "body"
p     = tag "p"
i     = tag "i"
b     = tag "b"
h1    = tag "h1"
h2    = tag "h2"
h3    = tag "h3"
h4    = tag "h4"
ol    = tag "ol"
ul    = tag "ul"
table = tag "table"
tr    = tag "tr"
th    = tag "th"

doc' :: HTML ()
doc' =
   html $ do
       head $ do
           title $ indentedString "Hello, world!"
       body $ do
           h1 $ indentedString "Greetings"
           p $ indentedString "Hello, world!"
{-
*Main> putStr $ render doc'
<html>
    <head>
        <title>
            Hello, world!
        </title>
    </head>
    <body>
        <h1>
            Greetings
        </h1>
        <p>
            Hello, world!
        </p>
    </body>
</html>
*Main> 
-}

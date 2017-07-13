module JValue (JValue (..)) where
import System.Environment (getArgs)

data JValue = JObject [(String,JValue)]
            | JArray  [JValue]
            | JNumber Float
            | JString String
            | JBool   Bool
            | JNull
            deriving Eq

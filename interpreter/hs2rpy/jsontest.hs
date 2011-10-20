

import Text.JSON.Generic
import Text.JSON.Pretty

main = do
        let x = toJSString "whaat"      -- JSString
            y = JSString x              -- JSValue
            z = JSArray [y,y,y,y]

        putStrLn $ show $ pp_value z






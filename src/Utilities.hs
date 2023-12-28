module Utilities where 

import Types(ValueType(..))


-- Utility function for defining type for array
getTypeForArray :: ValueType -> String 
getTypeForArray (ValueType value)
    | value == "0" = "Array of Date"
    | value == "1" = "Array of Number"
    | value == "2" = "Array of String"
    | value == "3" = "Array of Bool"
    | otherwise = "Unknown"
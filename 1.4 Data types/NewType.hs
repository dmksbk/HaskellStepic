module NewType where

-- newtype A a b = A b 		-- Good
-- newtype A = A 			-- Bad: The constructor of a newtype must have exactly one field but ‘A’ has none
-- newtype A = A A A 		-- Bad: The constructor of a newtype must have exactly one field but ‘A’ has two
-- newtype A = A a 			-- Bad: Not in scope: type variable ‘a’
-- newtype A a b = A a b 	-- Bad: The constructor of a newtype must have exactly one field but ‘A’ has two
-- newtype A a = A 			-- Bad: The constructor of a newtype must have exactly one field but ‘A’ has none
-- newtype A = A A 			-- Good
-- newtype A a b = A a 		-- Good
-- newtype A a = A a a 		-- The constructor of a newtype must have exactly one field but ‘A’ has two
-- newtype A a = A a 		-- Good
{-----------------------------------------------------------------
 
  (c) 2008-2009 Markus Dittrich 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | parser for error message of parser [sic]
module ErrorParser ( extract_error_position ) where


-- imports
import Prelude


-- local imports
import TokenParser


-- | extract the position of a parse error from the parsec
-- generated error string
extract_error_position :: String -> IO (Maybe Integer)
extract_error_position message = 
  case runParser error_parser () "" message of
    Left _  -> return Nothing
    Right i -> return $ Just i


-- | parser for extracting the location of a parse error from
-- the parse string
error_parser :: CharParser () Integer
error_parser = symbol "(line" *> integer *> symbol ", column"
               *> integer <* symbol "):"




------------------------------------------------------------------------------
--                                 GnadeLite                                --
--                                                                          --
--                         Copyright (C) 2006-2007                          --
--                      Pascal Obry - Olivier Ramonat                       --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package DB.Tools is

   use Ada.Strings.Unbounded;

   function F (F : in Float) return String;
   pragma Inline (F);
   --  Returns float image

   function I (Int : in Integer) return String;
   pragma Inline (I);
   --  Returns Integer image

   function Q (Str : in String) return String;
   pragma Inline (Q);
   --  Quotes the string and doubles all single quote in Str
   --  to be able to insert a quote into the database.
   --  Returns Null if empty string

   function Q (Str : in Unbounded_String) return String;
   pragma Inline (Q);
   --  As above but from an Unbounded_String

   function Q (Bool : in Boolean) return String;
   pragma Inline (Q);
   --  As above but for a boolean

end DB.Tools;

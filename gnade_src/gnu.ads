-------------------------------------------------------------------------------
--                                                                           --
--                            GNU Ada tools Bindings                         --
--                                                                           --
--                                   GNU                                     --
--                                                                           --
--                                 S P E C                                   --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 1999-2001
--  by Juergen Pfeifer
--
--  GNU Ada tools are free software; you can redistribute them and/or modify --
--  them under terms of the  GNU General Public License as published by the  --
--  Free Software  Foundation;  either version 2,  or (at your option) any   --
--  later version. GNU Ada tools are distributed in the hope that they will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the  implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--  GNU General Public License for  more details.  You should have received  --
--  a copy of the GNU General Public License  distributed with GNAT Ada      --
--  tools;  see file COPYING.  If not, write to  the                         --
--  Free Software Foundation,  59 Temple Place - Suite 330,  Boston,         --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
-------------------------------------------------------------------------------
--  Author: Juergen Pfeifer <juergen.pfeifer@gmx.net>
-------------------------------------------------------------------------------
--
--  This is the root of all the GNU Ada tools Bindings.
--
--  If you intend to put your own package under the GNU hierarchy, please
--  contact us in advance. Our intention is to develop a coherent and
--  comprehensive set of packages under this root package, so there is
--  a need for some coordination.
--
package GNU is
   pragma Pure (GNU);

   Version : constant String := "$Id: gnu.ads,v 1.2 2003/05/16 19:51:48 merdmann Exp $";

end GNU;

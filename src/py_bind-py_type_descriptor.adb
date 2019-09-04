------------------------------------------------------------------------------
--                              ada-py-bind                                 --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body Py_Bind.Py_Type_Descriptor is

   -------------
   -- Get_Arg --
   -------------

   function Get_Arg (Args : Py_Args; Index : Positive) return Ada_Type
   is
      Dummy : PyObject;
   begin
      declare
         P_Obj : constant PyObject := Get_Item (Args, Index);
         A_Obj : constant Ada_Type := To_Ada (P_Obj);
      begin
         Py_DECREF (P_Obj);
         return A_Obj;
      end;
   exception
      when E : others =>
         Dummy := Handle_Error (E);
         raise;
   end Get_Arg;

   -----------------
   -- P_To_Python --
   -----------------

   function P_To_Python (Self : Ada_T) return Py_Object_Ref is
      Ret : Py_Object_Ref;
   begin
      Py_Object_Smart_Ptr.Set (Ret, P_To_Python_Unsafe (Self));
      return Ret;
   end P_To_Python;

end Py_Bind.Py_Type_Descriptor;

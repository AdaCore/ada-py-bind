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

package body Py_Bind.Py_Property is

   package body From_Descriptor is

      procedure Init;

      ----------------
      -- Raw_Getter --
      ----------------

      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
      is
      begin
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            P    : constant Py_Object'Class :=
              Val_Desc.To_Python (Getter (Self.all));
         begin
            return P.Py_Data;
         end;
      end Raw_Getter;

      ----------------
      -- Raw_Setter --
      ----------------

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
      is
         Ignore : PyObject;
      begin
         Type_Check (Prop, Val_Desc.Py_Type, "property " & Property_Name);

         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            Val  : constant Val_Desc.Ada_Type := Val_Desc.To_Ada (Prop);
         begin
            Setter (Self.all, Val);
            return 0;
         end;
      exception
         when E : others =>
            Ignore := Handle_Error (E);
            return 1;
      end Raw_Setter;

      ----------
      -- Init --
      ----------

      procedure Init is
         Dummy : Boolean :=
           PyDescr_NewGetSet
             (Typ     => Self_Val.Val_Desc.P_Py_Type,
              Name    => Property_Name,
              Setter  => Raw_Setter'Unrestricted_Access,
              Getter  => Raw_Getter'Unrestricted_Access,
              Doc     => "");
      begin
         null;
      end Init;

   begin
      Init;
   end From_Descriptor;

   package body From_Py_Value is

      procedure Init;

      ----------------
      -- Raw_Getter --
      ----------------

      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
      is
      begin
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            Acc : constant Val_Desc.Access_Desc.Ada_T := Getter (Self);
            P    : constant Py_Object'Class :=
              Val_Desc.To_Python (Acc);
         begin
            return P.Py_Data;
         end;
      exception
         when E : others =>
            return Handle_Error (E);
      end Raw_Getter;

      ----------------
      -- Raw_Setter --
      ----------------

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
      is
         Ignore : PyObject;
      begin

         Type_Check (Prop, Val_Desc.Py_Type, "property " & Property_Name);

         --  We use a declare block to ensure that exception in To_Ada
         --  functions are caught.
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            Val  : constant Val_Desc.Access_Desc.Ada_T :=
              Val_Desc.To_Ada (Prop);
         begin
            Setter (Self, Val);
            return 0;
         end;
      exception
         when E : others =>
            Ignore := Handle_Error (E);
            return 1;
      end Raw_Setter;

      ----------
      -- Init --
      ----------

      procedure Init is
         Dummy : Boolean := PyDescr_NewGetSet
           (Typ     => Self_Val.Val_Desc.P_Py_Type,
            Name    => Property_Name,
            Setter  => Raw_Setter'Unrestricted_Access,
            Getter  => Raw_Getter'Unrestricted_Access,
            Doc     => "");
      begin
         null;
      end Init;

   begin
      Init;
   end From_Py_Value;

   package body Read_Only is

      procedure Setter
        (Self : in out Self_Val.Val_Desc.Ada_T; Val  : Val_Desc.Ada_Type);

      ------------
      -- Setter --
      ------------

      procedure Setter
        (Self : in out Self_Val.Val_Desc.Ada_T; Val  : Val_Desc.Ada_Type)
      is
      begin
         Runtime_Error ("Read only property");
      end Setter;

      package Dummy_Internal
      is new From_Descriptor (Val_Desc, Property_Name, Getter, Setter);
   end Read_Only;

end Py_Bind.Py_Property;

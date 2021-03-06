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

with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Interfaces.C;
with System;

package body Py_Bind.Py_Value is

   type T_Access is access all T;

   procedure Init;
   --  Initialization of the module. Called at elaboration

   procedure Set_UD (Self : Val_Access; Obj : PyObject);
   --  Put `Self` in `Obj` as a PyCapsule, in the `__userdata` field

   function To_Python
     (Self              : Val_Access;
      Owns_Ptr          : Boolean := True) return T;
   --  Constructor: Creates a python object from `Self`.

   function Default_Constructor
     (Data : PyObject; Args : PyObject) return PyObject with Convention => C;
   --  Default constructor Python function.

   function Default_Destructor
     (Data : PyObject; Args : PyObject) return PyObject with Convention => C;
   --  Default destructor Python function.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Val,
      Name   => Val_Access);

   Class   : PyObject;

   function Py_Type return PyObject is (Class);

   ------------
   -- Set_UD --
   ------------

   procedure Set_UD (Self : Val_Access; Obj : PyObject) is
      S : constant PyObject := PyCObject_FromVoidPtr (Self.all'Address);
   begin
      PyObject_SetAttrString (Obj, "__userdata", S);
   end Set_UD;

   ----------
   -- Free --
   ----------

   overriding procedure Destroy (Self : in out T) is
      R : Val_Access := To_Ada (Self.Py_Data);
   begin
      Destroy (R.all);
      if Self.Owns_Data then
         Free (R);
      end if;
      Destroy (Self);
   end Destroy;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Self : Val_Access) return Py_Object'Class is
   begin
      return To_Python (Self, False);
   end To_Python;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Self : Val) return Py_Object'Class is
   begin
      return To_Python (new Val'(Self), True);
   end To_Python;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Self : Val_Access; Owns_Ptr : Boolean := True) return T
   is
      Obj  : PyObject;
      Args : PyObject;
      New_Method : constant PyObject :=
        PyObject_GetAttrString (Class, "__new__");
      Init_Method : constant PyObject :=
        PyObject_GetAttrString (Class, "__init__");
      T_Inst : T_Access;
   begin

      --  Creating a new instance is equivalent to calling its metaclass. This
      --  is true for both new-style classes and old-style classes (for which
      --  the tp_call slot is set to PyInstance_New.

      --  Here, we are in fact calling  Class.__new__ (cls, *args, **kwargs).
      --  After allocating memory, this in turns automatically tp_init in the
      --  type definition, which in the case of GNATCOLL cases is often set to
      --  slot_tp_init. The latter in turn calls __init__

      Args := PyTuple_New (1);
      PyTuple_SetItem (Args, 0, Class);
      Py_INCREF (Class);

      Obj := PyObject_Call
        (Object => New_Method,
         Args   => Args,
         Kw     => null);   --  NOT: Py_None, which is not a valid dictionary
      Py_DECREF (New_Method);

      if Obj = null then
         if Active (Me) then
            Trace (Me, "Could not create instance");
            PyErr_Print;    --  debugging only
         end if;
         return No_T;
      end if;

      if Active (Me) then
         Assert
           (Me, Get_Refcount (Obj) = 1,
            "Object's refcount should be 1, got "
            & Get_Refcount (Obj)'Img,
            Raise_Exception => False);
      end if;

      --  The PyObject should have a single reference in the end, owned by
      --  the class instance itself.

      --  Set Self as user data on python object
      Set_UD (Self, Obj);

      Py_DECREF (Class);
      PyTuple_SetItem (Args, 0, Obj);
      Py_INCREF (Obj);

      declare
         Dummy : PyObject := PyObject_Call
           (Object => Init_Method,
            Args   => Args,
            Kw     => null);
      begin
         Py_DECREF (Args);
         Py_DECREF (Init_Method);
      end;

      T_Inst := new T'(Py_Data   => Obj,
                       Owns_Data => Owns_Ptr);

      return T_Inst.all;
   end To_Python;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Val_Access is
      UD          : PyObject;
   begin
      if PyObject_HasAttrString (Self, "__userdata") then
         UD := PyObject_GetAttrString (Self, "__userdata");

         --  Get the PyCapsule object embedded in the __userdata field

         declare
            A : constant System.Address := PyCObject_AsVoidPtr (UD);
            R : Val_Access with Import;
            for R'Address use A'Address;
         begin
            return R;
         end;
      end if;
      return null;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Val is
   begin
      return To_Ada (Self).all;
   end To_Ada;

   -------------------------
   -- Default_Constructor --
   -------------------------

   function Default_Constructor
     (Data : PyObject; Args : PyObject) return PyObject
   is
      pragma Unreferenced (Data);
      R : Val_Access;
      Self : PyObject;

   begin
      --  Check arguments
      declare
         Spec : constant Py_Fn_Profile := Create_Profile
           ((1 => Arg_Spec
             (Name    => "Self",
              Py_Type => Py_Type, Is_Kw => False, Doc => "")));
         Dummy_Args : constant Py_Args := Create (Args, null, Spec);
      begin
         null;
      end;

      if Args /= null then
         Self := PyObject_GetItem (Args, 0);
         if Self /= null then
            R := To_Ada (Self);
            if R = null then
               declare
                  V : constant Val := Create;
               begin
                  R := new Val'(V);
               end;
               Set_UD (R, Self);
            end if;
         end if;
      end if;

      return Py_None;
   exception
         when E : others => return Handle_Error (E);
   end Default_Constructor;

   ------------------------
   -- Default_Destructor --
   ------------------------

   function Default_Destructor
     (Data : PyObject; Args : PyObject) return PyObject
   is
      pragma Unreferenced (Data);
   begin
      --  Check arguments
      declare
         Spec       : constant Py_Fn_Profile := Create_Profile
           ((1 => Arg_Spec
             (Name    => "Self",
              Py_Type => Py_Type, Is_Kw => False, Doc => "")));
         Dummy_Args : constant Py_Args := Create (Args, null, Spec);
      begin
         null;
      end;

      return Py_None;
   exception
         when E : others => return Handle_Error (E);
   end Default_Destructor;

   ----------
   -- Init --
   ----------

   procedure Init is

      use Interfaces.C.Strings;

      Dict        : constant PyDictObject := PyDict_New;
      Ignored     : Integer;
      S           : chars_ptr;
      Constructor, Destructor : PyMethodDef;
   begin
      PyDict_SetItemString
        (Dict, "__module__",
         PyObject_GetAttrString (Module.Desc.Module, "__name__"));

      Class := Type_New
        (Name  => Name,
         Bases => null,
         Dict  => Dict);

      if Class = null then
         PyErr_Print;
         raise Program_Error
           with "Could not register class " & Name;
      end if;

      Py_INCREF (Class);

      Constructor :=
        Create_Method_Def
          ("__init__", Default_Constructor'Unrestricted_Access);

      Destructor :=
        Create_Method_Def
          ("__del__", Default_Destructor'Unrestricted_Access);

      Add_Method (Class, Constructor, Module => Module.Desc.Module);
      Add_Method (Class, Destructor, Module => Module.Desc.Module);

      S := New_String (Name);
      Ignored := PyModule_AddObject (Module.Desc.Module, S, Class);
      Free (S);
   end Init;

begin
   Init;
end Py_Bind.Py_Value;

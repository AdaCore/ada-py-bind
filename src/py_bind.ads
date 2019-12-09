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

with GNATCOLL.Python; use GNATCOLL.Python;
with GNATCOLL.Traces;
with GNATCOLL.Refcount;
with GNATCOLL.Strings;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;

package Py_Bind is

   use GNATCOLL;
   use GNATCOLL.Traces;

   type Py_Object is tagged record
      Py_Data : PyObject := Py_None;
   end record;

   procedure Destroy (Self : in out Py_Object);
   procedure Release (Self : in out Py_Object'Class);

   package Py_Object_Smart_Ptr
   is new Refcount.Shared_Pointers (Py_Object'Class, Release);

   subtype Py_Object_Ref is Py_Object_Smart_Ptr.Ref;

   Me : Trace_Handle := Create ("Py_Bind", From_Config);

   type Py_Arg_Spec is private;
   --  Specification for the argument of a python function.

   type Py_Arg_Spec_Array is array (Positive range <>) of Py_Arg_Spec;
   No_Args : constant Py_Arg_Spec_Array;

   function Arg_Spec
     (Name    : String;
      Py_Type : PyObject;
      Is_Kw   : Boolean := False;
      Doc     : String := "") return Py_Arg_Spec;
   --  Constructor for a python arg spec.

   type Py_Fn_Profile (Nb_Args : Natural) is private;
   --  Profile for a python function, containing specification of all the
   --  arguments.

   function Create_Profile
     (Args     : Py_Arg_Spec_Array;
      Ret_Type : PyObject := null) return Py_Fn_Profile;
   --  Create a fresh ``Py_Fn_Profile`` instance from an array of arg specs.

   function Empty_Profile return Py_Fn_Profile
   is (Create_Profile (No_Args));
   --  Return an empty profile.

   type Py_Args (Nb_Args : Natural) is tagged private;
   --  Encapsulates the arguments passed to a function, along with the profile
   --  of the function. This object is mainly used to check that the args
   --  passed conform to the specification of the function.

   function Create
     (Args, KwArgs : PyObject;
      Profile      : Py_Fn_Profile) return Py_Args;
   --  Create a new ``Py_Args`` object, from a profile, and the arguments
   --  passed to the function. Calling this constructor will check that the
   --  actuals match the specification.

   function Min_Args (Args : Py_Args) return Natural;
   --  Return the minimum number of arguments for ``Args``.

   function Max_Args (Args : Py_Args) return Natural;
   --  Return the maximum number of arguments for ``Args``.

   function Get_Item (Args : Py_Args; Index : Positive) return PyObject;
   --  Return the item at position ``Index`` in ``Args``.

   type Module_Descriptor is private;
   --  Describes a Python module.

private

   type Init_Fn is access procedure;

   package Init_Fn_Vectors is new Ada.Containers.Vectors (Positive, Init_Fn);
   subtype Init_Fn_Vector is Init_Fn_Vectors.Vector;

   type Module_Descriptor is record
      Init_Fns : Init_Fn_Vector;
      Module : PyObject;
   end record;

   No_Module_Descriptor : Module_Descriptor :=
     (Init_Fn_Vectors.Empty_Vector, Py_None);

   function Image (Obj : PyObject) return String;
   --  Utility function to print a PyObject

   Py_Type_Error : constant PyObject;
   Py_Runtime_Error : constant PyObject;
   Py_Index_Error : constant PyObject;
   --  Python built-in exception objects. Meant to be used along with
   --  ``Set_Error``.

   procedure Set_Error
     (Error : PyObject; Msg : String) renames PyErr_SetString;
   --  Set the current python error to ``Error``, with message ``Msg``.

   procedure Runtime_Error (Msg : String) with No_Return;
   --  Raise a python runtime error with the given message.

   procedure Type_Error (Msg : String) with No_Return;
   --  Raise a python type error with the given message.

   procedure Index_Error (Msg : String) with No_Return;
   --  Raise a python index error with the given message.

   Python_Bubble_Up : exception;
   --  Raise this exception if you want to exit and bubble up the current
   --  python error.

   function Handle_Error (E : Exception_Occurrence) return PyObject;

   --------------------------------------------------------------
   -- Description of python functions arguments specifications --
   --------------------------------------------------------------

   package Name_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash, "=");

   type Py_Arg_Spec is record
      Name    : Unbounded_String;
      --  The name of this argument.

      Py_Type : PyObject;
      --  The python type expected for this argument.

      Is_Kw   : Boolean := False;
      --  Whether the argument is a keyword argument or not.

      Doc     : Unbounded_String := Null_Unbounded_String;
      --  Documentation for this argument.
   end record;
   --  Specification for the argument of a python function.

   No_Args : constant Py_Arg_Spec_Array (1 .. 0) := (others => <>);

   type Py_Fn_Profile (Nb_Args : Natural) is record
      Args      : Py_Arg_Spec_Array (1 .. Nb_Args);
      --  Arguments of the function.

      Ret_Type  : PyObject := null;
      --  Return type for the function.

      Valid_Kws : Name_Sets.Set;
      --  Set of valid keyword argument's names.
   end record;

   function Min_Args (Args : Py_Fn_Profile) return Natural;
   --  Return the minimum number of arguments for this profile.

   function Max_Args (Args : Py_Fn_Profile) return Natural;
   --  Return the maximum number of arguments for this profile.

   Empty_Args_Spec : Py_Fn_Profile (0) := (0, others => <>);

   type Py_Fn_Profile_Access is access all Py_Fn_Profile;

   type Py_Args (Nb_Args : Natural) is new Py_Object with record
      KwArgs       : PyObject;
      Args_Spec    : Py_Fn_Profile_Access;
      Matched_Args : PyObject_Array (1 .. Nb_Args);
   end record;

   procedure Type_Check
     (Val, Expected_Type : PyObject;
      Target_Desc        : String);
   --  Helper to type check a value against a python type. ``Target_Desc`` that
   --  is used in the error message to describe the target. For example, for a
   --  method argument named "A", it might be "argument A".

   overriding procedure Destroy (Self : in out Py_Args);

   pragma Import (C, Py_Type_Error, "PyExc_TypeError");
   pragma Import (C, Py_Runtime_Error, "PyExc_RuntimeError");
   pragma Import (C, Py_Index_Error, "PyExc_IndexError");

end Py_Bind;

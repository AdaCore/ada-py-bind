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

   Python_Type_Error    : exception;
   Python_Bounds_Error  : exception;

   procedure Type_Error (Msg : String);
   --  Raise a python type error with the given message.

   procedure Index_Error (Msg : String);
   --  Raise a python index error with the given message.

   function Handle_Error (E : Exception_Occurrence) return PyObject;

   --------------------------------------------------------------
   -- Description of python functions arguments specifications --
   --------------------------------------------------------------

   package Name_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash, "=");

   type Py_Arg_Spec is record
      Name    : Unbounded_String;
      Py_Type : PyObject;
      Is_Kw   : Boolean := False;
      Doc     : Unbounded_String := Null_Unbounded_String;
   end record;

   function Arg_Spec
     (Name  : String; Py_Type : PyObject;
      Is_Kw : Boolean := False; Doc : String := "") return Py_Arg_Spec
   is
     (Py_Arg_Spec'(To_Unbounded_String (Name), Py_Type, Is_Kw,
                   To_Unbounded_String (Doc)));

   type Py_Arg_Spec_Array is array (Positive range <>) of Py_Arg_Spec;
   Empty_Arg_Spec_Array : Py_Arg_Spec_Array (1 .. 0) := (others => <>);

   type Py_Args_Spec (Nb_Args : Natural) is limited record
      Args      : Py_Arg_Spec_Array (1 .. Nb_Args);
      Valid_Kws : Name_Sets.Set;
   end record;
   --  Profile for a python function, containing specification of all the
   --  arguments, as well as valid keyword arg values.

   function Create (Args : Py_Arg_Spec_Array) return Py_Args_Spec;
   --  Create a fresh ``Py_Args_Spec`` instance from an array of arg specs

   function Min_Args (Args : Py_Args_Spec) return Natural;
   --  Return the minimum number of arguments

   function Max_Args (Args : Py_Args_Spec) return Natural;

   Empty_Args_Spec : Py_Args_Spec (0) := (0, others => <>);

   type Py_Args_Spec_Access is access all Py_Args_Spec;

   type Py_Args (Nb_Args : Natural) is new Py_Object with record
      KwArgs       : PyObject;
      Args_Spec    : Py_Args_Spec_Access;
      Matched_Args : PyObject_Array (1 .. Nb_Args);
   end record;

   function Create
     (Args, KwArgs : PyObject;
      Args_Spec    : Py_Args_Spec) return Py_Args;

   function Min_Args (Args : Py_Args) return Natural
   is (Min_Args (Args.Args_Spec.all));
   function Max_Args (Args : Py_Args) return Natural
   is (Max_Args (Args.Args_Spec.all));
   function Get_Item (Args : Py_Args; Index : Positive) return PyObject;

   overriding procedure Destroy (Self : in out Py_Args);

end Py_Bind;

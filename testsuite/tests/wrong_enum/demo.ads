with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Python; use GNATCOLL.Python;

with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;
with Py_Bind.Py_Functions;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   type Point is record
      X, Y : Integer := 12;
   end record;

   function Get_X (Self : Point) return Integer;
   procedure Set_X (Self : in out Point; X : Integer);
   function Get_Y (Self : Point) return Integer;
   procedure Set_Y (Self : in out Point; Y : Integer);

   type Shape_Kind is range 1 .. 3;

   type Shape is record
      Id : Unbounded_String;
      Kind : Shape_Kind := 1;
      Position : Point;
   end record;

   procedure Translate (Self : in out Shape; Move : Point);

   function Get_Id (Self : Shape) return String;
   procedure Set_Id (Self : in out Shape; Id : String);

   function Get_Kind (Self : Shape) return Shape_Kind;
   procedure Set_Kind (Self : in out Shape; Kind : Shape_Kind);

   ---------------------
   -- Python bindings --
   ---------------------

   package Module is new Py_Module ("gen");

   --  Bind point
   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Point_X_Prop is new Py_Point_Prop.Byval
     (Int_Type, "x", Get_X, Set_X);
   package Py_Shape_Y_Prop is new Py_Point_Prop.Byval
     (Int_Type, "y", Get_Y, Set_Y);

   --  Bind shape
   package Py_Shape is new Py_Value (Shape, Module, "Shape");
   package Py_Shape_Prop is new Py_Property (Py_Shape);

   package Py_Shape_Id_Prop is new Py_Shape_Prop.Byval
     (String_Type, "id", Get_Id, Set_Id);

   function Get_Position (S : Py_Shape.Val_Access) return Py_Point.Val_Access;
   procedure Set_Position
     (S : Py_Shape.Val_Access; Point : Py_Point.Val_Access);

   package Py_Shape_Position_Prop is new Py_Shape_Prop.Byref
     (Py_Point.Access_Desc, "position", Get_Position, Set_Position);

   package Py_Shape_Kind_Desc is new Simple_Enum_Binding (Shape_Kind);

   package Py_Shape_Kind_Prop is new Py_Shape_Prop.Byval
     (Py_Shape_Kind_Desc.Type_Desc, "kind", Get_Kind, Set_Kind);

   function Dummy_Method (Args : Py_Args) return PyObject;
   function Py_Translate (Args : Py_Args) return PyObject;

   package Py_Shape_Dummy_Method is new Py_Bind.Py_Functions.Raw_Method
     ("dummy", Dummy_Method, Py_Shape, Doc => "Dummy doc");

   package Py_Shape_Translate is new Py_Bind.Py_Functions.Raw_Method
     ("translate", Py_Translate, Py_Shape,
      Doc => "Dummy doc",
      Profile => Create_Profile ((Arg_Spec ("self", Py_Shape.Py_Type),
                                  Arg_Spec ("move", Py_Point.Py_Type))));

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;

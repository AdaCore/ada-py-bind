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

   type Shape is record
      Id       : Unbounded_String;
      Position : Point;
   end record;

   function Get_Id (Self : Shape) return String;
   procedure Set_Id (Self : in out Shape; Id : String);
   procedure Translate (Self : in out Shape; Move : Point);

   function Shape_From_JSON (JSON : String) return Shape;
   function Shape_To_JSON (Self : Shape) return String;

   ---------------------
   -- Python bindings --
   ---------------------

   package Module is new Py_Module ("gen");

   ------------
   --  Point --
   ------------

   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Point_X_Prop is new Py_Point_Prop.Byval
     (Int_Type, "x", Get_X, Set_X);
   package Py_Shape_Y_Prop is new Py_Point_Prop.Byval
     (Int_Type, "y", Get_Y, Set_Y);

   ------------
   --  Shape --
   ------------

   package Py_Shape is new Py_Value (Shape, Module, "Shape");
   package Py_Shape_Prop is new Py_Property (Py_Shape);

   package Py_Shape_Id_Prop is new Py_Shape_Prop.Byval
     (String_Type, "id", Get_Id, Set_Id);

   function Get_Position (S : Py_Shape.Rec_Access) return Py_Point.Rec_Access;
   procedure Set_Position
     (S : Py_Shape.Rec_Access; Point : Py_Point.Rec_Access);
   package Py_Shape_Position_Prop is new Py_Shape_Prop.Byref
     (Py_Point.Access_Desc, "position", Get_Position, Set_Position);

   function Dummy_Method (Args : Py_Args) return PyObject;
   function Py_Translate (Args : Py_Args) return PyObject;
   function Py_Shape_From_JSON (Args : Py_Args) return PyObject;
   function Py_Shape_To_JSON (Args : Py_Args) return PyObject;

   package Py_Shape_Dummy_Method is new Py_Bind.Py_Functions.Raw_Method
     ("dummy", Dummy_Method, Py_Shape, Doc => "Dummy doc",
      Profile => Create_Profile (No_Args));

   package Py_Shape_Translate is new Py_Bind.Py_Functions.Raw_Method
     ("translate", Py_Translate, Py_Shape,
      Doc  => "Dummy doc",
      Profile => Create_Profile ((Arg_Spec ("self", Py_Shape.Py_Type),
                                  Arg_Spec ("move", Py_Point.Py_Type))));

   package Py_Shape_Shape_To_Json is new Py_Bind.Py_Functions.Raw_Method
     ("to_json", Py_Shape_To_JSON, Py_Shape,
      Doc  => "Dummy doc",
      Profile =>
         Create_Profile
        ((1 => Arg_Spec ("self", Py_Shape.Py_Type)),
         Ret_Type => String_Type.P_Py_Type));

   package Py_Shape_Shape_From_JSON is new Py_Bind.Py_Functions.Raw_Function
     ("from_json", Py_Shape_From_JSON, Module,
      Doc  => "Dummy doc",
      Profile => Create_Profile
        ((1 => Arg_Spec ("json", String_Type.P_Py_Type)),
         Ret_Type => Py_Shape.Py_Type));

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;

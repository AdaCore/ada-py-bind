with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   pragma Suppress (Access_Check);

   type Point is record
      X, Y : Integer := 12;
   end record;

   function Get_X (Self : Point) return Integer;
   procedure Set_X (Self : in out Point; X : Integer);
   function Get_Y (Self : Point) return Integer;
   procedure Set_Y (Self : in out Point; Y : Integer);

   type Shape is record
      Id : Unbounded_String;
      Position : Point;
   end record;

   function Get_Id (Self : Shape) return String;
   procedure Set_Id (Self : in out Shape; Id : String);

   ---------------------
   -- Python bindings --
   ---------------------

   package Test_Module is new Py_Module ("test");

   --  Bind point
   package Py_Point is new Py_Value (Point, Test_Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Point_X_Prop is new Py_Point_Prop.Byval
     (Int_Type, "x", Get_X, Set_X);
   package Py_Shape_Y_Prop is new Py_Point_Prop.Byval
     (Int_Type, "y", Get_Y, Set_Y);

   --  Bind shape
   package Py_Shape is new Py_Value (Shape, Test_Module, "Shape");
   package Py_Shape_Prop is new Py_Property (Py_Shape);

   package Py_Shape_Id_Prop is new Py_Shape_Prop.Byval
     (String_Type, "id", Get_Id, Set_Id);

   function Get_Position (S : Py_Shape.Rec_Access) return Py_Point.Rec_Access;
   procedure Set_Position
     (S : Py_Shape.Rec_Access; Point : Py_Point.Rec_Access);
   package Py_Shape_Position_Prop is new Py_Shape_Prop.Byref
     (Py_Point.Access_Desc, "position", Get_Position, Set_Position);

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "inittest");
end Demo;

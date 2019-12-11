with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;
with Py_Bind.Desc_Array;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   type Coords_Kind is (X, Y, Z);
   type Coords_Type is array (Coords_Kind) of Integer;

   type Point is record
      Coords : Coords_Type := (0, 0, 0);
   end record;

   ---------------------
   -- Python bindings --
   ---------------------

   package Module is new Py_Module ("gen");

   --  Bind point
   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Coords_Index is new Simple_Enum_Binding (Coords_Kind);

   package Py_Coords is new Py_Bind.Desc_Array
     (Coords_Kind, Py_Coords_Index.Type_Desc, Integer,
      Int_Type, Coords_Type, Module, "Coords");

   function Get_Coords
     (P : Py_Point.Val_Access)
      return Py_Coords.Py_Container.Val_Access;

   procedure Set_Coords
     (P : Py_Point.Val_Access; Val : Py_Coords.Py_Container.Val_Access);

   package Py_Point_Coords_Prop is new Py_Point_Prop.From_Py_Value
     (Py_Coords.Py_Container, "coords", Get_Coords, Set_Coords);

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;

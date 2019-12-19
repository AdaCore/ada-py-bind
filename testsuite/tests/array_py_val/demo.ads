with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Py_Value;
with Py_Bind.Py_Property;
with Py_Bind.Types; use Py_Bind.Types;
with Py_Bind.Desc_Array;
with Py_Bind.Py_Val_Array;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   subtype Coords_Index is Integer range 1 .. 3;

   type Coords_Type is array (Coords_Index) of Integer;

   type Point is record
      Coords : Coords_Type := (0, 0, 0);
   end record;

   subtype Points_Index is Integer range 1 .. 10;
   type Points_Array is array (Integer range 1 .. 10) of Point;

   type Plot is record
      Name   : Unbounded_String;
      Points : Points_Array;
   end record;

   ---------------------
   -- Python bindings --
   ---------------------

   function Create return Point is (others => <>);

   package Module is new Py_Module ("gen");

   --  Bind point
   package Py_Point is new Py_Value (Point, Module, "Point");
   package Py_Point_Prop is new Py_Property (Py_Point);

   package Py_Coords_Index is new Simple_Discrete_Binding (Coords_Index);

   package Py_Coords is new Desc_Array
     (Index_Type   => Coords_Index,
      Index_Desc   => Py_Coords_Index.Type_Desc,
      Element_Type => Integer,
      Element_Desc => Py_Bind.Types.Int_Type,
      Array_Type   => Coords_Type,
      Module       => Module,
      Name         => "Coords");

   function Get_Coords
     (P : Py_Point.Val_Access) return Py_Coords.Py_Container.Val_Access;

   procedure Set_Coords
     (P : Py_Point.Val_Access; Val : Py_Coords.Py_Container.Val_Access);

   package Py_Point_Coords_Prop is new Py_Point_Prop.From_Py_Value
     (Py_Coords.Py_Container, "coords", Get_Coords, Set_Coords);

   function Create return Plot is (others => <>);

   package Py_Plot is new Py_Value (Plot, Module, "Plot");

   package Py_Plot_Prop is new Py_Property (Py_Plot);

   package Py_Point_Array is new Py_Val_Array (Index_Type   => Points_Index,
                                               Index_Desc   => Int_Type,
                                               Element_Type => Point,
                                               Element_Val  => Py_Point,
                                               Array_Type   => Points_Array,
                                               Module       => Module,
                                               Name         => "PointArray");

   function Get_Points
     (P : Py_Plot.Val_Access) return Py_Point_Array.Py_Container.Val_Access;

   procedure Set_Points
     (P : Py_Plot.Val_Access; Val : Py_Point_Array.Py_Container.Val_Access);

   package Py_Plot_Points_Prop is new Py_Plot_Prop.From_Py_Value
     (Val_Desc      => Py_Point_Array.Py_Container,
      Property_Name => "points",
      Getter        => Get_Points,
      Setter        => Set_Points);

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;

Require Import List.

Fixpoint zipWith {A B C: Type}(f: A -> B -> C) (xs: list A )(ys: list B) : list C:=
match xs,ys with
| nil,_ => nil
| _,nil => nil
| (x::xs'),(y::ys') => (f x y) :: (zipWith f xs' ys')
end.
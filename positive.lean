inductive Pos : Type where
  | one : Pos
  | succ (n : Pos) : Pos 

class Plus (α : Type) where
  plus : α → α → α


instance : Plus Nat where 
  plus := Nat.add


open Plus (plus)
open Pos

#eval plus 5 6

def add_pos (α:Pos) (β : Pos) : Pos :=
  match α with
  | one => succ β
  | succ k => succ (add_pos k β)


instance : Plus Pos where
  plus := add_pos


#eval plus (succ one) (one)


instance : Plus Float where 
plus := Float.add


structure Btree where
  root : Nat
  left : Option Btree
  right : Option Btree
  deriving Repr


def tree : Btree := {root:=5,left := none,right := none}

#eval tree

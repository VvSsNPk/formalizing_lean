structure Btree where
  val : Option Int
  left : Option Btree
  right : Option Btree

def btre : Btree := {val := none, left:=none, right:=none}

def Btree.MkBtree (α : Option Int) : Btree :=
  {val := α , left:= none,right:= none}

def Btree.Insert (α : Int) (β : Btree) : Btree :=
  match β.val with
  | none => {val := some α,left := none,right:= none}
  | some x => if x >= α then
    match β.left with
    | some y => {val:= some x,left := Insert α y,right:=β.right}
    | none => {val := some α,left := none,right:=none}
    else 
    match β.right with
    | some y => {val:= some x,left:= β.left,right:=Insert α y}
    | none => {val:= some α,left:=none,right:=none}
